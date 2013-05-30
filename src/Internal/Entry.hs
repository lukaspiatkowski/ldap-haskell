module Internal.Entry where

import Data.Char
import Data.List
import Encoding.LDAPDefs

type LDAPRDN = String

data Entry = Entry LDAPRDN AttributeList deriving (Eq, Show)

mAK :: String -> Attribute -> Bool
mAK key (k, _) = k == key

mAV :: String -> Attribute -> Bool
mAV value (_, vs) = any (value==) vs

getRDN :: Entry -> Maybe Attribute
getRDN (Entry rdnKey attrs) = find (mAK rdnKey) attrs

getAttribute :: String -> AttributeList -> String
getAttribute key attrs = case find (mAK key) attrs of Just (_, value:_) -> value

ldapDnToList :: LDAPDN -> [(String, String)]
ldapDnToList = map (toPairOn '=') . splitOn ',' where
  splitOn c s = if any (c ==) s then
    let (w, (_:s')) = break (c ==) s in w:splitOn c s' else
    [s]
  toPairOn c s = let (w, (_:s')) = break (c ==) s in (w, s')

dnListToFunctions :: [(String, String)] -> [Entry -> Bool]
dnListToFunctions = map crtMtchRdn where
  crtMtchRdn (rdn, value) entry = rdnPredicate rdn (value==) entry

createEntry :: (String, String) -> AttributeList -> Entry
createEntry (rdn, value) attrs = let
    fun (k, vs) = if k == rdn then
      (k, if any (==value) vs then vs else value:vs) else
      (k, vs)
    newAttrs = if any (mAK rdn) attrs then
      map fun attrs else
      (rdn, [value]):attrs
  in Entry rdn newAttrs

entryPathToString :: [Entry] -> (LDAPDN, AttributeList)
entryPathToString entries =
  (concat $ intersperse "," $ map entryRdnToString entries,
  case entries of (Entry _ attrs:_) -> attrs) where
    entryRdnToString (Entry rdn attrs) = rdn ++ ('=':(getAttribute rdn attrs))

rdnPredicate :: String -> (String -> Bool) -> Entry -> Bool
rdnPredicate rdn p (Entry eRdn attrs) = rdn == eRdn &&
  case find (mAK rdn) attrs of
    Just (_, vs) -> all p vs
    Nothing -> False

attributesPredicate :: (AttributeList -> Bool) -> Entry -> Bool
attributesPredicate p (Entry _ attrs) = p attrs
