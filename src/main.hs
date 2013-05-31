module Main where

import Network
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Concurrent
import Control.Monad
import  qualified Data.ByteString as DBS
import Data.Word
import Data.Maybe

import Encoding.BER
import Encoding.LDAPDefs
import Encoding.LDAPRead
import Encoding.LDAPWrite

import Internal.Api

maybeAnd :: [Maybe Bool] -> Maybe Bool
maybeAnd mb =
    case all (\x -> x = Just True) mb of
       True -> Just True
       False ->
           case any (\x -> x = Just False) of
               True -> Just False
               False -> Nothing

maybeOr :: [Maybe Bool] -> Maybe Bool
maybeOr mb =
    case any (\x -> x = Just True) mb of
       True -> Just True
       False ->
           case all (\x -> x = Just False) of
               True -> Just False
               False -> Nothing

genFilter :: Filter -> [Attribute] -> Maybe Bool
genFilter f =
    case f of
        And fl = (\x -> maybeAnd $ map (flip . genFilter $ x) fl
        Or fl =  (\x -> maybeOr $ map (flip . genFilter $ x) fl
        Not fil = not $ genFilter fil
        EqualityMatch (attr, val) = \x -> Just $ any (\(a, vl) -> a == attr && any ((==) val) vl) x
        SubstringsF sf = \x -> Nothing
        GreaterOrEqual ava = \x -> Nothing
        LessOrEqual ava = \x -> Nothing
        Present (attr, val) = \x -> Just $ any (\(a, vl) -> a == attr) x
        ApproxMatch = \x -> Nothing
        ExtensibleMatch = \x -> Nothing

filterGenerator :: Filter -> [Attribute] -> Bool
filterGenerator f a =
    case genFilter f a of
         Nothng -> False
         Just x -> x


bindUserResp :: LDAPMessage -> LDAPMessage
bindUserResp (id, BindRequest protV dn authCh, ctrls) = 
    let f x = (id, BindResponse (x, dn, "", Nothing) Nothing, Nothing) in
    case protV of
       3 -> f Success
       _ -> f ProtocolError

searchResponse :: LDAPMessage -> LDAPInternalState -> [LDAPMessage]
searchResponse (id, SearchRequest dn scope deref i1 i2 b filter attrs, ctrls) state =
   let results = search dn scope (filterGenerator filter) state in
   [(id, SearchResultDone (Success, "", "", Nothing), Nothing)]

modifyResponse :: LDAPMessage -> (LDAPMessage, LDAPInternalState)
modifyResponse (id, ModifyRequest dn opl, ctrls) =
    let ns = modify dn opl in
    ((id, ModifyResponse (Success, "", "", Nothing), Nothing), ns)

addResponse :: LDAPMessage -> (LDAPMessage, LDAPInternalState)
addResponse (id, AddRequest entr attrs, ctrls) =
    let ns = add entr attrs in
    ((id, AddResponse (Success, "", "", Nothing), Nothing), ns)

delResponse :: LDAPMessage -> (LDAPMessage, LDAPInternalState)
delResponse (id, DelRequest dn, ctrls) =
    let ns = remove dn in 
    ((id, DelResponse (Success, "", "", Nothing), Nothing), ns)

readMessage :: Handle -> BERMessage -> IO BERTree
readMessage handle msg =
    case sizeLeft msg of
        Nothing -> return $ buildTree msg
        Just x -> do
            msg_data <- DBS.hGet handle x
            readMessage handle $ BERData $ berData msg ++ DBS.unpack msg_data

constructResp :: LDAPMessage -> Maybe [[Word8]]
constructResp ldap_msg@(id, op, ctrls) =
    case op of
        BindRequest {} -> Just [bindUserResp ldap_msg]
        SearchRequest {} -> Just $ searchResponse ldap_msg
        ModifyRequest {} -> Just [modifyResponse ldap_msg]
        AddRequest {} -> Just [addResponse ldap_msg]
        DelRequest {} -> Just [delResponse ldap_msg]
        UnbindRequest -> Nothing
        _ -> Just [ldapToBer (id, op, ctrls)]


respondMsg :: Handle -> Either String LDAPMessage -> IO Bool
respondMsg handle ldap_msg =
    case ldap_msg of
        Left s -> return True
        Right msg -> case constructResp msg of
                       Nothing -> return False
                       Just msg_data -> do
                                     forM_ msg_data (DBS.hPut handle . DBS.pack)
                                     return True

loop socket = do
    (handle, _, _) <- accept socket
    loop_comm handle
    --print "end loop"
    loop socket
    where
        loop_comm handle = do
            intermediate_msg <- readMessage handle newMessage
            --print $ show intermediate_msg
            print $ show $ buildLDAP intermediate_msg
            continue <- respondMsg handle $ buildLDAP intermediate_msg
            if continue
                then loop_comm handle 
                else do
                    hFlush handle
                    hClose handle

data Flag = ListenPort Int
          | Help
          deriving (Show, Eq, Ord)

listenPort :: String -> Flag
listenPort = ListenPort . read

flags = [
         Option ['p'] [] (ReqArg listenPort "PORT" )
           "Set the port number(defult is 5002)"
        ,Option ['h'] ["help"] (NoArg Help)
           "Print this help message"
        ]

parse argv = case getOpt Permute flags argv of
               (args, fs, []) ->
                  if Help `elem` args
                     then do hPutStrLn stderr (usageInfo header flags)
                             exitSuccess
                     else return args
               (_, _, errs) -> do
                     hPutStrLn stderr (concat errs ++ usageInfo header flags)
                     exitWith (ExitFailure 1)
             where header = "Usage ldap_srv [-p PORT] [-h]"

getPort :: [Flag] -> Int
getPort l = case l of
             ListenPort i : _ -> i
             h:t -> getPort t
             [] -> 5002

main = withSocketsDo $ do
    --default port 636
    args <- getArgs >>= parse
    socket <- listenOn $ PortNumber $ toEnum $ getPort args 
    loop socket
