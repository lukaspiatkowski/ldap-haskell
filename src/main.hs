module Main where

import Network
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Protocol.LDAPProtocol

loop socket state = do
    (handle, _, _) <- accept socket
    rstate <- loop_comm handle state
    --print "end loop"
    loop socket rstate
    where
        loop_comm handle lstate = do
            intermediate_msg <- readMessage handle
            --print $ show intermediate_msg
            print $ show $ intermediate_msg
            (continue, nstate) <- respondMsg handle lstate intermediate_msg
            if continue
                then loop_comm handle nstate
                else do
                    hFlush handle
                    hClose handle
                    return nstate

data Flag = ListenPort Int
          | Help
          deriving (Show, Eq, Ord)

flags = [
         Option ['p'] [] (ReqArg (ListenPort . read) "PORT" )
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
    loop socket []
