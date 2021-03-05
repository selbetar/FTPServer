module Main where

import Commands
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 (unpack)
import Data.Maybe
import Network.Socket
import qualified Network.Socket.ByteString as S
import SockNetwork
import System.Environment
import System.IO
import Text.Read
import UserState
import Util

defaultPortNum :: String
defaultPortNum = "5000"

main :: IO ()
main = do
  args <- getArgs
  let port = parseArgs args
  if port == ""
    then do
      help
    else do
      sock <- createSock port
      pr <- socketPort sock
      putStrLn ("--> FTPServer has started on port " ++ show pr)
      mainLoop sock

mainLoop :: Socket -> IO b
mainLoop sock = do
  conn <- accept sock
  runConn conn
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  sendLine sock "220 Welcome"
  interactFTP sock (UserState "" "" False "" "" NoConnection ASCII)
  putStrLn "--> done"
  return ()

-- listen for commands from client
interactFTP :: Socket -> UserState -> IO UserState
interactFTP sock userState =
  do
    -- print userState
    E.catch
      ( do
          words <- S.recv sock 1024
          let line = C8.unpack words
          putStr ("<-- " ++ line)
          let cmd = strToUpper (getFirst (tokens line))
              paramList = tail (tokens line)
          state <- executeCommand sock userState cmd paramList
          if cmd == "QUIT"
            then do return state
            else do interactFTP sock state
      )
      ( \e ->
          do
            let err = show (e :: E.IOException)
            hPutStrLn stderr ("--> Error (interactFTP): " ++ err)
            hPutStrLn stderr "--> Closing Control Connection"
            close sock
            return userState
      )
  where
    tokens :: String -> [String]
    tokens str = splitsep (`elem` " \t\n\f\r\v") str

-- returns the port number if provided
-- if no ports specified, returns default port
-- if an non-numerical value is given, returns an empty string
parseArgs :: [String] -> String
parseArgs args
  | null args = defaultPortNum
  | otherwise =
    if isJust $ maybePort portNum
      then portNum
      else ""
  where
    portNum = head args
    maybePort port = readMaybe portNum :: Maybe Int

help :: IO ()
help =
  do
    progName <- getProgName
    putStrLn $ "Usage:  " ++ progName ++ " <port>"
    putStrLn "Specifiy the port number the server should listen to for incoming connections."
    putStrLn "Or leave it empty for the server to listen on the default port."
    return ()