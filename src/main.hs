module Main where

import Network
import System.IO
import Control.Concurrent
import  qualified Data.ByteString as DBS
--import Data.List

import Encoding.BER
import Encoding.LDAPDefs
import Encoding.LDAPRead
import Encoding.LDAPWrite

bindUserResp (id, BindRequest protV dn authCh, ctrls) = 
    let f = \x ->  ldapToBer (id, BindResponse (x, dn, "", Nothing) Nothing, Nothing) in
    case protV of
       3 -> f Success
       _ -> f ProtocolError



readMessage handle msg =
    case sizeLeft msg of
        Nothing -> do
            print $ berData msg
            return $ buildTree msg
        Just x -> do
            msg_data <- DBS.hGet handle x
            readMessage handle (BERData $ (berData msg) ++ (DBS.unpack msg_data))

constructResp ldap_msg@(id, op, ctrls) =
    case op of
        BindRequest protV dn autchCh -> bindUserResp ldap_msg
        _ -> ldapToBer (id+1, op, ctrls)


respondMsg handle ldap_msg =
    case ldap_msg of
        Left s -> print s
        Right msg -> DBS.hPut handle $ DBS.pack $ constructResp msg

loop socket = do
    (handle, _, _) <- accept socket
    forkIO $ body handle
    loop socket
    where
     body handle = do
        --na razie tylko wypisujaemy ber na wyjście
        intermediate_msg <- readMessage handle newMessage
        --buildLDAP intermediate_msg
        print $ show intermediate_msg
        print $ show $ buildLDAP intermediate_msg
        respondMsg handle $ buildLDAP intermediate_msg
        print "end loop"
        body handle

main = withSocketsDo $ do
    --na razie port jest hardcoded, później jako opcja uruchamiania
    socket <- listenOn $ PortNumber 5002
    loop socket
