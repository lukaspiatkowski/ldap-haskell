import Control.Monad.State (runState)
import Data.List
import Encoding.LDAPDefs
import Internal.LDAPInternalApi

main :: IO ()
main = let
    match key val attrs = case find (\(k, _) -> key == k) attrs of
      Just (_, vs) -> any (val==) vs
      Nothing -> False
    opers = [(Add, ("fn", ["Tam", "Sam"])), (Delete, ("sn", []))]
  in
    print $ fst $
      runState (search "ou=mim,ou=uw" WholeSubtree (match "fn" "Sam")) $
      snd $ runState (modify "cn=Lukasz,ou=mim,ou=uw" opers) $
      snd $ runState (add "cn=Lukasz,ou=mim,ou=uw" [("sn", ["Piatkowski"])]) []
