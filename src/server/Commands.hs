module Commands where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 
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
    rootDir :: FilePath, -- root directory (starting directory)
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
    UserState "user" "user" False "elwd" "" NoConnection ASCII
  ]

-- executeCommand executes an ftp command and send a reply to the client
executeCommand :: Socket -> UserState -> String -> [String] -> IO UserState
executeCommand sock state command paramLst
  | command == "USER" = do cmdUser sock state paramLst
  | command == "PASS" = do cmdPass sock state paramLst
  | command == "PASV" = do cmdPasv sock state paramLst
  | command == "PWD"  = do cmdPwd sock state paramLst
  | command == "RETR" = do cmdRetr sock state paramLst
  | command == "TYPE" = do cmdType sock state paramLst
  | command == "MODE" = do cmdMode sock state paramLst
  | command == "NOOP" = do cmdNoop sock state
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

-- cmdUser handles the USER command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- returns the UserState associated with the username sent.
cmdUser :: Socket -> UserState -> [String] -> IO UserState
cmdUser sock state params =
  if checkParams params 1
    then
      if doesUserExist username usersList
        then do
          uState <- getUserState username -- get the state associated with that user
          dir <- getCurrentDirectory
                      -- preserve transfer parameter
          let nuState = setTransType (getType state) $ setRootDir dir uState
          sendLine sock "331 Username OK. Send password."
          return nuState
        else do
          sendLine sock "530 User doesn't exist"
          return state
    else do
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    username = getFirst params

-- cmdPass handles the PASS command 
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- Modifies the UserState by setting the status of isLoggedIn
-- Or returns an empty state on an incorrect password.
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

cmdNoop :: Socket -> b -> IO b
cmdNoop sock state = do
  sendLine sock "200 Command okay."
  return state

cmdType :: Socket -> UserState -> [String] -> IO UserState
cmdType sock state params = do
  if checkParams params 1
    then
      if getIsLoggedin state
        then
          case transType of
            "A" -> do sendLine sock "200 Command okay."
                      return (setTransType ASCII state)
            "I" -> do sendLine sock "200 Command okay."
                      return (setTransType Binary state)
            _  -> do sendLine sock "504 Command not implemented for that parameter"
                     return state
        else do
          sendLine sock "530 Not logged in."
          return state
    else do 
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    transType = strToUpper (getFirst params)

cmdMode :: Socket -> UserState -> [String] -> IO UserState
cmdMode sock state params = do
  if checkParams params 1
    then
      if getIsLoggedin state
        then
          if mode == "S"
            then do
              sendLine sock "200 Command okay."
              return state -- always default mode
            else do
              sendLine sock "504 Command not implemented for that parameter"
              return state
        else do
          sendLine sock "530 Not logged in."
          return state
    else do 
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    mode = getFirst params
-- cmdPasv handles the PASV command 
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- Modifies the UserState by setting the dataSock.
cmdPasv :: Socket -> UserState -> [String] -> IO UserState
cmdPasv sock state params
  | getIsLoggedin state =
    if checkParams params 0
      then do
        dataSocket <- createSock "0"
        addrPortStr <- getAddrPort dataSocket
        sendLine sock ("227 Entering Passive Mode " ++ addrPortStr)
        return (setDataSock (DataSocket dataSocket) state)
      else do
        sendLine sock "501 PASV doesn't support any parameters."
        return state
  | otherwise =
    do
      sendLine sock "530 Log in first."
      return state

-- cmdPwd handles the PWD command
-- takes: controlSocket, UserState
-- Sends the name of current working directory to client
cmdPwd :: Socket -> UserState -> [String] -> IO UserState
cmdPwd sock state params
  | getIsLoggedin state =
    if checkParams params 0
      then do
        currDir <- getCurrentDirectory
        sendLine sock ("257 \"" ++ currDir ++ "\" is current directory.")
        return state
      else do
        sendLine sock "501 PWD doesn't support any parameters."
        return state
  | otherwise =
    do
      sendLine sock "530 Log in first."
      return state

-- cmdRetr handles the RETR command 
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- Modifies the UserState by setting the dataSock.
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
          | 'r' `notElem` getPerms state -> -- Check if the user allowed to invoke RETR
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
                    fHandle <- if transType == ASCII then do openFile filePath ReadMode else do openBinaryFile filePath ReadMode
                    iseof <- hIsEOF fHandle
                    if iseof -- check if the file is empty on open
                      then do
                        sendLine sock "450 Requested file was empty."
                        return state
                      else do
                        sendLine sock "150 File status okay; about to open data connection."
                        let dataSock = getSock (getDataSock state)
                        controlConn <- accept dataSock -- Open data connection
                        sendFile sock controlConn fHandle transType
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

-- sendFile sends a file over the data connection; RETR command helper
-- takes: controlSocket, (dataSock, dataSockAddr), and a file handle.
-- Returns true on a successful send, false otherwise.
sendFile :: Socket -> (Socket, SockAddr)-> Handle -> Type -> IO Bool
sendFile controlSock (dataSock,_) fHandle dataType =
  do
    if dataType == ASCII 
      then do 
        sendASCII controlSock dataSock fHandle
      else do
        putStrLn "Binary Mode"
        sendBinary controlSock dataSock fHandle


sendASCII :: Socket -> Socket -> Handle -> IO Bool
sendASCII controlSock dataSock fHandle =
  E.catch
    ( do
        iseof <- hIsEOF fHandle
        if iseof 
          then do 
            sendLine controlSock "226 Closing data connection. Requested file action successful."
            close dataSock
            return True
          else do 
            line <- BS.hGetLine fHandle
            S.sendAll dataSock (replaceEOL line)
            sendASCII controlSock dataSock fHandle
    )
    ( \e ->
        do
          let err = show (e :: E.IOException)
          hPutStrLn stderr ("--> Error (sendFile): " ++ err)
          sendLine controlSock "426 Connection closed; transfer aborted."
          return False
    )
  
  where
    bCR = (C8.pack "\r")
    bLF = (C8.pack "\n")
    replaceEOL :: BS.ByteString -> BS.ByteString
    replaceEOL bs = 
      if (BS.drop (BS.length bs - 1) bs) == bCR -- check if it ends in CR
        then 
          BS.append bs bLF
        else
          BS.append bs $ BS.append bCR bLF

sendBinary :: Socket -> Socket-> Handle -> IO Bool
sendBinary controlSock dataSock fHandle =
  E.catch
    ( do
        bytesRead <- BS.hGet fHandle 1024
        if bytesRead == BS.empty
          then do
            sendLine controlSock "226 Closing data connection. Requested file action successful."
            close dataSock
            hClose fHandle
            return True
          else do
          S.sendAll dataSock bytesRead
          sendBinary controlSock dataSock fHandle
    )
    ( \e ->
        do
          let err = show (e :: E.IOException)
          hPutStrLn stderr ("--> Error (sendFile): " ++ err)
          sendLine controlSock "426 Connection closed; transfer aborted."
          return False
    )

-- handleIOErrors checks the cause of an IOError that occurred due to
-- an IO command and report it to the client.
-- takes: controlSocket, and the raised error.
handleIOErrors :: Socket -> IOError -> IO ()
handleIOErrors sock err
  | isAlreadyInUseError err = do sendLine sock "450 Requested file action not taken. File is busy."
  | isDoesNotExistError err = do sendLine sock "550 Requested file action not taken. File doesn't exist."
  | isPermissionError err = do sendLine sock "550 Requested file action not taken. Access denied."
  | otherwise = do
    let errStr = show err
    hPutStrLn stderr errStr
    sendLine sock "550 Requested file action not taken."


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

setTransType :: Type -> UserState -> UserState
setTransType transType (UserState username password isLoggedIn permissions rootDir dataSock _)= 
  UserState username password isLoggedIn permissions rootDir dataSock transType

-- Sets the rootDir to dir only if rootDir is empty
-- in most cases, dir will be the directory where the server is running
setRootDir :: FilePath -> UserState -> UserState
setRootDir dir (UserState username password isLoggedIn permissions rootDir dataSock transType)
  | rootDir == "" = (UserState username password isLoggedIn permissions dir dataSock transType)
  | otherwise = (UserState username password isLoggedIn permissions rootDir dataSock transType)

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