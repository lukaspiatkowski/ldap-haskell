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

bindUserResp :: LDAPMessage -> [Word8]
bindUserResp (id, BindRequest protV dn authCh, ctrls) = 
    let f x = ldapToBer (id, BindResponse (x, dn, "", Nothing) Nothing, Nothing) in
    case protV of
       3 -> f Success
       _ -> f ProtocolError

searchResponse :: LDAPMessage -> [[Word8]]
searchResponse (id, SearchRequest dn scope deref i1 i2 b filter attrs, ctrls) =
   map ldapToBer [(id, SearchResultDone (Success, "", "", Nothing), Nothing)]

modifyResponse :: LDAPMessage -> [Word8]
modifyResponse (id, ModifyRequest dn opl, ctrls) =
    ldapToBer (id, ModifyResponse (Success, "", "", Nothing), Nothing)

addResponse :: LDAPMessage -> [Word8]
addResponse (id, AddRequest entr attrs, ctrls) =
    ldapToBer (id, AddResponse (Success, "", "", Nothing), Nothing)

delResponse :: LDAPMessage -> [Word8]
delResponse (id, DelRequest dn, ctrls) = 
    ldapToBer (id, DelResponse (Success, "", "", Nothing), Nothing)

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

listenPort :: Maybe String -> Flag
listenPort = ListenPort . read . fromMaybe "5002"

flags = [
         Option ['p'] [] (OptArg listenPort "PORT" )
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
             where header = "Usage ldap_srv [-pPORT] [-h]"

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
