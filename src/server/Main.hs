module Main where

import System.IO
import Control.Concurrent
import Network.Socket
import SockNetwork
import Commands

main :: IO ()
main = do
  sock <- createSock "5000"
  pr <- socketPort sock
  putStrLn (">> FTPServer has started on port " ++ (show pr))
  mainLoop sock

mainLoop :: Socket -> IO b
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn) -- split off each connection into its own thread
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  sendLine hdl "220 Welcome"
  interactFTP hdl (UserState "" "" False "" "" NoConnection)
  return ()

-- listen for commands from client
interactFTP :: Handle -> UserState -> IO UserState
interactFTP h userState = do
  print userState
  line <- hGetLine h
  putStrLn ("<< " ++ line)
  state <- executeCommand h userState (getFirst (tokens line)) (tail (tokens line))
  interactFTP h state
  where
    tokens :: String -> [String]
    tokens str = splitsep (`elem` " \t\n\f\r\v") str