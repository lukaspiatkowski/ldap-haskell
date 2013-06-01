module Protocol.LDAPProtocol where

import Data.Tree
import qualified Data.ByteString as DBS
import Control.Monad
import Control.Monad.State (evalState, execState)
import System.IO

import Encoding.BER
import Encoding.LDAPDefs
import Encoding.LDAPRead
import Encoding.LDAPWrite

import Internal.LDAPInternalApi
import Internal.Entry

import Debug.Trace
-- helper functions for transforming
-- a Filter nto a function of type [Attribute] -> Bool
maybeAnd :: [Maybe Bool] -> Maybe Bool
maybeAnd mb =
    case all (\x -> x == Just True) mb of
       True -> Just True
       False ->
           case any (\x -> x == Just False) mb of
               True -> Just False
               False -> Nothing

maybeOr :: [Maybe Bool] -> Maybe Bool
maybeOr mb =
    case any (\x -> x == Just True) mb of
       True -> Just True
       False ->
           case all (\x -> x == Just False) mb of
               True -> Just False
               False -> Nothing

maybeNot :: Maybe Bool -> Maybe Bool
maybeNot Nothing = Nothing
maybeNot (Just x) = Just (not x)

genFilter :: Filter -> [Attribute] -> Maybe Bool
genFilter f =
    let undef x = Nothing in
    case f of
        And fl -> (\x -> maybeAnd $ map (flip genFilter x) fl)
        Or fl ->  (\x -> maybeOr $ map (flip genFilter x) fl)
        Not fil -> (\x -> maybeNot $ genFilter fil x)
        EqualityMatch (attr, val) -> \x -> Just $ any (\(a, vl) -> a == attr && any ((==) val) vl) x
        SubstringsF sf -> undef
        GreaterOrEqual ava -> undef
        LessOrEqual ava -> undef
        Present attr -> \x -> Just $ any (\(a, vl) -> a == attr) x
        ApproxMatch ava -> undef
        ExtensibleMatch mra -> undef

filterGenerator :: Filter -> [Attribute] -> Bool
filterGenerator f a =
    case genFilter f a of
         Nothing -> False
         Just x -> x

-- handling specific LDAPMessages
bindUserResp :: LDAPMessage -> LDAPMessage
bindUserResp (id, BindRequest protV dn authCh, ctrls) = 
    let f x = (id, BindResponse (x, dn, "", Nothing) Nothing, Nothing) in
    case protV of
       3 -> f Success
       _ -> f ProtocolError

searchResponse :: LDAPMessage -> Forest Entry -> [LDAPMessage]
searchResponse (id, SearchRequest dn scope deref i1 i2 b filter attrs, ctrls) f =
   let results = evalState (search dn scope (filterGenerator filter)) f in
   let part_res = if i1 == 0 then results else take i1 results in
   let mapper (en, attrlst) = if b then (en, []) else (en, attrlst) in
   let checkAttrs (en, attrlst) = mapper (en, attrlst) in
   let toMessage (en, attrlst) = (id, SearchResultEntry en attrlst, ctrls) in
   let resp_list = map (toMessage . checkAttrs) part_res in
   (trace ("search" ++ show f) resp_list) ++ [(id, SearchResultDone (Success, "", "", Nothing), Nothing)]

modifyResponse :: LDAPMessage -> Forest Entry -> (LDAPMessage, Forest Entry)
modifyResponse (id, ModifyRequest dn opl, ctrls) st =
    let ns = execState (modify dn opl) st in
    ((id, ModifyResponse (Success, "", "", Nothing), Nothing), ns)

addResponse :: LDAPMessage -> Forest Entry -> (LDAPMessage, Forest Entry)
addResponse (id, AddRequest entr attrs, ctrls) st =
    let ns = execState (add entr attrs) st in
    trace ("add"++show ns) ((id, AddResponse (Success, "", "", Nothing), Nothing), ns)

delResponse :: LDAPMessage -> Forest Entry -> (LDAPMessage, Forest Entry)
delResponse (id, DelRequest dn, ctrls) st =
    let ns = execState (remove dn) st in
    ((id, DelResponse (Success, "", "", Nothing), Nothing), ns)

-- handling of response creation

constructResp :: LDAPMessage -> Forest Entry -> (Maybe [LDAPMessage], Forest Entry)
constructResp ldap_msg@(id, op, ctrls) state =
    let pToMSp f (r, s) = (Just $ f r, s) in
    case op of
        BindRequest {} -> pToMSp (:[]) (bindUserResp ldap_msg, state)
        SearchRequest {} -> (Just $ searchResponse ldap_msg state, state)
        ModifyRequest {} -> pToMSp (:[]) $ modifyResponse ldap_msg state
        AddRequest {} -> pToMSp (:[]) $ addResponse ldap_msg state
        DelRequest {} -> pToMSp (:[]) $ delResponse ldap_msg state
        UnbindRequest -> (Nothing, state)
        _ -> (Just [(id, op, ctrls)], state)


respondMsg :: Handle -> Forest Entry -> Either String LDAPMessage -> IO (Bool, Forest Entry)
respondMsg handle state ldap_msg =
    case ldap_msg of
        Left s -> return (True, state)
        Right msg -> case constructResp msg state of
               (Nothing, nst) -> return (False, nst)
               (Just msg_data, nst) -> do
                             forM_ msg_data (toMessage handle)
                             return (True, nst)
                             where
                                toMessage h = DBS.hPut h . DBS.pack . ldapToBer 


-- handling of reading messages

messageGetter :: Handle -> BERMessage -> IO BERTree
messageGetter handle msg =
    case sizeLeft msg of
        Nothing -> return $ buildTree msg
        Just x -> do
            msg_data <- DBS.hGet handle x
            messageGetter handle $ BERData $ berData msg ++ DBS.unpack msg_data

readMessage :: Handle -> IO (Either String LDAPMessage)
readMessage handle = do
    msg <- messageGetter handle newMessage
    return $ buildLDAP msg