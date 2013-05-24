module Entry where

import Data.Char
import Data.List

type Attribute = (String, String)

data Entry = Entry String [Attribute] deriving (Eq, Show)

mAK :: String -> Attribute -> Bool
mAK key (k, _) = k == key

mAV :: String -> Attribute -> Bool
mAV value (_, v) = v == value

getRDN :: Entry -> Maybe Attribute
getRDN (Entry rdnKey attrs) = find (mAK rdnKey) attrs

getAttribute :: Entry -> String -> Maybe Attribute
getAttribute (Entry _ attrs) key = find (mAK key) attrs

getAttributes :: Entry -> String -> [Attribute]
getAttributes (Entry _ attrs) key = filter (mAK key) attrs

getMatchingAttribute :: Entry -> Attribute -> Maybe Attribute
getMatchingAttribute entry (k, v) = case getAttribute entry k of
  Nothing -> Nothing
  Just attr -> if mAV v attr then Just attr else Nothing

getMatchingAttributes :: Entry -> Attribute -> [Attribute]
getMatchingAttributes entry (k, v) = filter (mAV v) $ getAttributes entry k
