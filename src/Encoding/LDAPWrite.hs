module Encoding.LDAPWrite where

import Encoding.InternalUtils
import Encoding.BER
import Encoding.LDAPDefs
import Data.Word
import qualified Codec.Binary.UTF8.String as Codec

ldapToBer :: LDAPMessage -> [Word8]
ldapToBer = writeTree . writeLDAP

writeLDAP :: LDAPMessage -> BERTree

writeLDAP (id, op, controls) = Sequence ((IntNode id):(writeOperation op):(writeControls controls))

writeOperation :: ProtocolOp -> BERTree
writeOperation (BindResponse result (Just creds)) =
    ApplicationSpecific 1 $ (berSequence $ writeResult result) ++ (ContextNode 7 $ map intToWord8 creds):[]

writeOperation (BindResponse result Nothing) =
    writeResultResp 1 result

writeOperation (SearchResultEntry dn partialAttrs) =
    ApplicationSpecific 4 [writeString dn, Sequence $ map writePartialAttr partialAttrs]

writeOperation (SearchResultDone result) =
    writeResultResp 5 result

writeOperation (SearchResultReference urls) =
    ApplicationSpecific 19 $ map writeString urls

writeOperation (ModifyResponse result) =
    writeResultResp 7 result

writeOperation (AddResponse result) =
    writeResultResp 9 result

writeOperation (DelResponse result) =
    writeResultResp 11 result

writeControls :: Maybe Controls  -> [BERTree]
writeControls Nothing = []
writeControls _ = error "Controls not supported"

writeResult :: LDAPResult -> BERTree
writeResult (code, dn, str, referal) =
    Sequence (codeL:dnL:strL:referalL)
    where
        codeL = Enumerated $ fromEnum code
        dnL = writeString dn
        strL = writeString str
        referalL = unmaybeLDAP writeReferal referal

writeResultResp :: Int -> LDAPResult -> BERTree
writeResultResp val args =
    ApplicationSpecific val (berSequence $ writeResult args)

unmaybeLDAP :: (a -> [BERTree]) -> Maybe a -> [BERTree]
unmaybeLDAP f Nothing = []
unmaybeLDAP f (Just a) = f a

writeString :: String -> BERTree
writeString = OctetString . Codec.encode

writeReferal = error "Referal unsapported"

writePartialAttr :: PartialAttribute -> BERTree
writePartialAttr (descr, vals ) =
    Sequence ([writeString descr, SetOf $ map writeString vals])
