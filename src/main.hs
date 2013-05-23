module Main where

import Network
import System.IO
import  qualified Data.ByteString as DBS
--import Data.List

import Encoding.BER
import Encoding.LDAPRead

readMessage handle msg =
    case sizeLeft msg of
        Nothing -> do return $ buildTree msg
        Just x -> do
            msg_data <- DBS.hGet handle x
            real_message <-readMessage handle (BERData $ (berData msg) ++ (DBS.unpack msg_data))
            return real_message

loop socket = do
    (handle, _, _) <- accept socket
    --na razie tylko wypisujaemy ber na wyjście
    intermediate_msg <- readMessage handle newMessage
    --buildLDAP intermediate_msg
    System.IO.putStrLn $ show $ buildLDAP intermediate_msg
    loop socket

main = withSocketsDo $ do
    --na razie port jest hardcoded, później jako opcja uruchamiania
    socket <- listenOn $ PortNumber 5002
    loop socket
