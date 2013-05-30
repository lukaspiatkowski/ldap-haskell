module LDAPInternalApi where

import Control.Monad.State
import Data.Tree
import Encoding.LDAPDefs
import Internal.Entry
import qualified Internal.Tree as Tree

type LDAPInternalState a = State (Forest Entry) a

add :: LDAPDN -> AttributeList -> LDAPInternalState ()
add dn attrs = let
    (rdn:rdns) = ldapDnToList dn
    filters = dnListToFunctions $ reverse rdns
    entry = createEntry rdn attrs
    updateForest [] _ forest = forest
    updateForest ((rdn, value):rdns) dn forest = let
        matchRdn = rdnPredicate rdn (value==)
        addFilters = dnListToFunctions dn
        searchFilters = addFilters ++ dnListToFunctions [(rdn, value)]
        updatedForest = Tree.add addFilters (createEntry (rdn, value) []) forest
      in updateForest rdns (dn ++ [(rdn, value)])
        (case Tree.search searchFilters matchRdn BaseObject forest of
          [] -> updatedForest 
          _ -> forest)
  in do
    forest <- get
    put $ Tree.add filters entry $ updateForest (reverse rdns) [] forest

remove :: LDAPDN -> LDAPInternalState ()
remove dn = do
  forest <- get
  put $ Tree.remove (dnListToFunctions $ reverse $ ldapDnToList dn) forest

modify :: LDAPDN -> [(Operation, PartialAttribute)] -> LDAPInternalState ()
modify = undefined

search :: LDAPDN -> SearchScope -> (AttributeList -> Bool)
  -> LDAPInternalState [(LDAPDN, AttributeList)]
search = undefined
