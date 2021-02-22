module Commands where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8 (pack, unpack)
import Data.Char
import Network.Socket
import qualified Network.Socket.ByteString as S
import SockNetwork
import System.IO
import System.IO.Error
import System.Directory

-- username password isLoggedIn permissions rootDir dataSock
data UserState = UserState
  { username :: String,
    password :: String,
    isLoggedIn :: Bool,
    permissions :: String, -- user access permissions
    rootDir :: String, -- starting directory of the user
    dataSock :: DataConnection, -- data connection socket
    transferType :: Type -- Transfer type
  }
  deriving (Show)

data DataConnection = NoConnection | DataSocket Socket
  deriving (Show, Eq)

data Type = ASCII | Binary
  deriving (Show, Eq)

cCRLF :: [Char]
cCRLF = "\r\n"

-- List of supported commands
commands :: [String]
commands =
  [ "USER",
    "PASS",
    "CWD",
    "CDUP",
    "PASV",
    "PWD",
    "QUIT",
    "NLST",
    "TYPE",
    "STRU",
    "RETR",
    "STOR",
    "MODE",
    "NOOP"
  ]

usersList :: [UserState]
usersList =
  [ UserState "test" "test" False "elrwd" "" NoConnection ASCII,
    UserState "user" "user" False "elrwd" "" NoConnection ASCII
  ]

-- executeCommand executes an ftp command and send a reply to the client
executeCommand :: Socket -> UserState -> String -> [String] -> IO UserState
executeCommand sock state command paramLst
  | command == "USER" = do cmdUser sock state paramLst
  | command == "PASS" = do cmdPass sock state paramLst
  | command == "PASV" = do cmdPasv sock state paramLst
  | command == "RETR" = do cmdRetr sock state paramLst
  | otherwise = do
    sendLine sock "502 Command not implemented"
    return state

-- sendLine sends a line over the control connection to the client
-- Throws an exception if an error occurs while sending
sendLine :: Socket -> String -> IO ()
sendLine sock str =
  E.catch
    ( do
        putStrLn ("--> " ++ str)
        S.sendAll sock (C8.pack (str ++ cCRLF))
    )
    ( \e ->
        do
          let err = show (e :: E.IOException)
          hPutStrLn stderr ("--> Error (sendLine): " ++ err)
          E.throw e -- propagte the exception to exit interactFTP loop
    )

cmdUser :: Socket -> UserState -> [String] -> IO UserState
cmdUser sock state params =
  if checkParams params 1
    then
      if doesUserExist username usersList
        then do
          -- update the state
          uState <- getUserState username
          sendLine sock "331 Username OK. Send password."
          return uState
        else do
          sendLine sock "530 User doesn't exist"
          return state
    else do
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    username = getFirst params

-- TODO: set working directory
cmdPass :: Socket -> UserState -> [String] -> IO UserState
cmdPass sock state params =
  if checkParams params 1
    then
      if getUsername state == ""
        then do
          sendLine sock "503 Login with a user first."
          return state
        else
          if passwordActual == passwordRecived
            then do
              sendLine sock "230 User logged in, proceed."
              return (setIsLoggedIn True state)
            else do
              sendLine sock "530 Incorrect password"
              return (UserState "" "" False "" "" NoConnection ASCII)
    else do
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    passwordRecived = getFirst params
    passwordActual = getPass state

cmdPasv :: Socket -> UserState -> [String] -> IO UserState
cmdPasv sock state params
  | getIsLoggedin state =
    if checkParams params 0
      then do
        dataSocket <- createSock "0"
        sockAddr <- getSocketName dataSocket
        addrPortStr <- getAddrPort sockAddr
        sendLine sock ("227 Entering Passive Mode " ++ addrPortStr)
        return (setDataSock (DataSocket dataSocket) state)
      else do
        sendLine sock "501 PASV doesn't support any parameters."
        return state
  | otherwise =
    do
      sendLine sock "530 Log in first."
      return state

-- 550 Requested action not taken.
-- 425 Can't open data connection.
-- 426 Connection closed; transfer aborted
-- 451 Requested action aborted: local error in processing.
-- 450 Requested file action not taken. File unavailable (e.g., file busy).
-- * 125 Data connection already open; transfer starting.
-- 150 File status okay; about to open data connection.
-- 226 Closing data connection. Requested file action successful.
cmdRetr :: Socket -> UserState -> [String] -> IO UserState
cmdRetr sock state params
  | getIsLoggedin state =
    if checkParams params 1
      then case () of
        ()
          | getDataSock state == NoConnection ->
            do
              sendLine sock "425 Data connection was not established."
              return state
          | 'r' `notElem` getPerms state ->
            do
              putStrLn (show state)
              sendLine sock "550 Requested action not taken. Not Allowed."
              return state
          | otherwise ->
            do
              let transType = getType state
              E.catch
                ( do
                    let filePath = getFirst params
                    fd <- if transType == ASCII then do openFile filePath ReadMode else do openBinaryFile filePath ReadMode
                    iseof <- hIsEOF fd
                    if iseof
                      then do
                        sendLine sock "450 Requested file was empty."
                        return state
                      else do
                        sendLine sock "150 File status okay; about to open data connection."
                        -- Open control connection
                        let dataSock = getSock (getDataSock state)
                        controlConn <- accept dataSock
                        sendFile sock controlConn fd
                        return (setDataSock NoConnection state)
                )
                ( \e ->
                    do 
                      let err = e :: IOError
                      handleIOErrors sock err
                      return state
                )
      else do
        sendLine sock "501 Syntax error in parameters or arguments."
        return state
  | otherwise =
    do
      sendLine sock "530 Log in first."
      return state
  where
    getSock (DataSocket sock) = sock

sendFile :: Socket -> (Socket, SockAddr)-> Handle -> IO Bool
sendFile controlSock (dataSock,_) fd =
  E.catch
    ( do
        fileContent <- BS.hGetContents fd
        S.sendAll dataSock fileContent
        sendLine controlSock "226 Closing data connection. Requested file action successful."
        close dataSock
        return True
    )
    ( \e ->
        do
          let err = show (e :: E.IOException)
          hPutStrLn stderr ("--> Error (sendFile): " ++ err)
          sendLine controlSock "426 Connection Closed. Transfer aborted."
          return False
    )


handleIOErrors :: Socket -> IOError -> IO ()
handleIOErrors sock err
  | isAlreadyInUseError err = do sendLine sock "450 Requested file action not taken. File is busy."
  | isDoesNotExistError err = do sendLine sock "550 Requested file action not taken. File doesn't exist."
  | isPermissionError err = do sendLine sock "550 Requested file action not taken. Access denied."
  | otherwise = do
    let errStr = show err
    hPutStrLn stderr errStr
    sendLine sock "550 Requested file action not taken."


getAddrPort :: SockAddr -> IO String
getAddrPort (SockAddrInet portNum hostAddr) =
  do
    let (h1, h2, h3, h4) = hostAddressToTuple hostAddr
        host = formatResponse [h1, h2, h3, h4]
        ports = formatResponse [portNum `div` 256, portNum `mod` 256]
        npa = "(" ++ host ++ init ports ++ ")."
    putStrLn ("--> " ++ npa)
    return npa
  where
    formatResponse lst = foldr (\x y -> show x ++ "," ++ y) [] lst

-- Takes a username, and returns the inital state of that user that is stored in
-- usersList
-- Assumes that username exists in the list of usersList
getUserState :: String -> IO UserState
getUserState name =
  return
    ( head
        (filter (\(UserState username _ _ _ _ _ _) -> name == username) usersList)
    )

doesUserExist :: String -> [UserState] -> Bool
doesUserExist user [] = False
doesUserExist user users = (user == username) || doesUserExist user (tail users)
  where
    username = getUsername (head users)

setIsLoggedIn :: Bool -> UserState -> UserState
setIsLoggedIn status (UserState username password _ permissions rootDir dataSock transType) =
  UserState username password status permissions rootDir dataSock transType

setDataSock :: DataConnection -> UserState -> UserState
setDataSock dataConn (UserState username password isLoggedIn permissions rootDir _ transType) =
  UserState username password isLoggedIn permissions rootDir dataConn transType

getUsername :: UserState -> String
getUsername (UserState username _ _ _ _ _ _) = username

getPass :: UserState -> String
getPass (UserState _ password _ _ _ _ _) = password

getIsLoggedin :: UserState -> Bool
getIsLoggedin (UserState _ _ isLoggedIn _ _ _ _) = isLoggedIn

getPerms :: UserState -> String
getPerms (UserState _ _ _ permissions _ _ _) = permissions

getRootDir :: UserState -> String
getRootDir (UserState _ _ _ _ rootDir _ _) = rootDir

getDataSock :: UserState -> DataConnection
getDataSock (UserState _ _ _ _ _ dataSock _) = dataSock

getType :: UserState -> Type
getType (UserState _ _ _ _ _ _ transferType) = transferType

-- takes a parameter list, and checks if it has the expected number of
-- parameters, which is specified by count
checkParams :: [String] -> Int -> Bool
checkParams params count = length params == count

isCmdValid :: [Char] -> Bool
isCmdValid cmd = strToUpper cmd `elem` commands

-- returns the first element of a list of strings
-- or empty if there are no elements
getFirst :: [String] -> String
getFirst [] = []
getFirst (h : t) = h

-- converts a string to upper case
strToUpper :: [Char] -> [Char]
strToUpper = map toUpper

-- splitsep takes a Boolean separator function, a list and constructs a list of the elements between the separators.
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep f lst = filter isNotEmpty (split lst [])
  where
    split [] [] = []
    split [] lst = [lst]
    split (h : t) lst
      | f h = lst : split t []
      | otherwise = split t (lst ++ [h])

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty lst = True