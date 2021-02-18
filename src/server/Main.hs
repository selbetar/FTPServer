module Main where

import Commands
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8 (unpack)
import Network.Socket
import qualified Network.Socket.ByteString as S
import SockNetwork
import System.IO

main :: IO ()
main = do
  sock <- createSock "5000"
  pr <- socketPort sock
  putStrLn ("--> FTPServer has started on port " ++ show pr)
  mainLoop sock

mainLoop :: Socket -> IO b
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn) -- split off each connection into its own thread
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  sendLine sock "220 Welcome"
  interactFTP sock (UserState "" "" False "" "" NoConnection)
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
          state <- executeCommand sock userState (strToUpper (getFirst (tokens line))) (tail (tokens line))
          interactFTP sock state
      )
      ( \e ->
          do
            let err = show (e :: E.IOException)
            hPutStrLn stderr ("--> Error (interactFTP): " ++ err)
            hPutStrLn stderr "--> Closing Control Connection"
            return userState
      )
  where
    tokens :: String -> [String]
    tokens str = splitsep (`elem` " \t\n\f\r\v") str
