module Commands where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Network.Socket
import qualified Network.Socket.ByteString as S
import SockNetwork
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import UserState
import Util

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
    "MODE",
    "NOOP"
  ]

cCRLF :: [Char]
cCRLF = "\r\n"

-- executeCommand executes an ftp command and send a reply to the client
executeCommand :: Socket -> UserState -> String -> [String] -> IO UserState
executeCommand sock state command paramLst
  | command == "USER" = do cmdUser sock state paramLst
  | command == "PASS" = do cmdPass sock state paramLst
  | command == "PASV" = do cmdPasv sock state paramLst
  | command == "QUIT" = do cmdQuit sock state
  | command == "NLST" = do cmdNlst sock state paramLst
  | command == "STRU" = do cmdStru sock state paramLst
  | command == "PWD" = do cmdPwd sock state paramLst
  | command == "RETR" = do cmdRetr sock state paramLst
  | command == "TYPE" = do cmdType sock state paramLst
  | command == "MODE" = do cmdMode sock state paramLst
  | command == "NOOP" = do cmdNoop sock state
  | command == "CDUP" = do cmdCdup sock state paramLst
  | command == "CWD" = do cmdCwd sock state paramLst
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
      if getIsLoggedin state
        then do
          sendLine sock "530 Changing user during an active session is not allowed."
          return state
        else do
          if doesUserExist username usersList
            then do
              uState <- getUserState username -- get the state associated with that user
              dir <- canonicalizePath ""
              -- preserve transfer parameter and set rootDir
              let nuState = setTransType (getType state) (setRootDir dir uState)
              setCurrentDirectory (getRootDir nuState)
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

cmdNoop :: Socket -> UserState -> IO UserState
cmdNoop sock state = do
  sendLine sock "200 Command okay."
  return state

-- cmdType handles the TYPE command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- Modifies the UserState by setting the transfer type
cmdType :: Socket -> UserState -> [String] -> IO UserState
cmdType sock state params = do
  if checkParams params 1
    then
      if getIsLoggedin state
        then case transType of
          "A" -> do
            sendLine sock "200 Command okay."
            return (setTransType ASCII state)
          "I" -> do
            sendLine sock "200 Command okay."
            return (setTransType Binary state)
          _ -> do
            sendLine sock "504 Command not implemented for that parameter"
            return state
        else do
          sendLine sock "530 Not logged in."
          return state
    else do
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    transType = strToUpper (getFirst params)

-- cmdMode handles the MODE command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
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
    mode = strToUpper $ getFirst params

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

-- cmdQuit handles the QUIT command
-- takes: controlSocket, UserState
-- Closes the control connection
cmdQuit :: Socket -> UserState -> IO UserState
cmdQuit sock state =
  do
    sendLine sock "221 Service closing control connection."
    close sock
    return state

-- cmdNlst handles the NLST command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
-- Sends directory listing from server to client.
cmdNlst :: Socket -> UserState -> [String] -> IO UserState
cmdNlst sock state params
  | getIsLoggedin state =
    case () of
      ()
        | getDataSock state == NoConnection ->
          do
            sendLine sock "425 Data connection was not established."
            return state
        | 'l' `notElem` getPerms state -> -- Check if the user is allowed to invoke NLST
          do
            sendLine sock "550 Requested action not taken. Not Allowed."
            return state
        | otherwise -> do
          case () of
            ()
              | checkParams params 0 ->
                do
                  let dataSock = getSock (getDataSock state)
                  currDir <- getCurrentDirectory
                  listFiles sock dataSock currDir
                  return state
              | checkParams params 1 ->
                do
                  let dataSock = getSock (getDataSock state)
                  rootDir <- canonicalizePath (getRootDir state)
                  pathName <- canonicalizePath (getFirst params)
                  if isSubdirectory rootDir pathName
                    then do
                      listFiles sock dataSock pathName
                      return state
                    else do
                      sendLine sock "550 Requested action not taken. Not Allowed."
                      return state
              | otherwise ->
                do
                  sendLine sock "501 Syntax error in parameters or arguments."
                  return state
  | otherwise =
    do
      sendLine sock "530 Log in first."
      return state
  where
    getSock (DataSocket sock) = sock

-- listFiles sends list of directories to client over the data connection
listFiles :: Socket -> Socket -> FilePath -> IO ()
listFiles controlSock dataSock pathName =
  do
    E.catch
      ( do
          entries <- listDirectory pathName
          sendLine controlSock "150 File status okay; about to open data connection."
          dataConn <- accept dataSock -- Open data connection
          let (dataSock, _) = dataConn
          foldMap (sendFile dataSock) entries
          sendLine controlSock "226 Closing data connection. Requested file action successful."
          close dataSock
      )
      ( \e ->
          do
            let err = e :: IOError
            handleIOErrors controlSock err
      )
  where
    sendFile sock entry =
      E.catch
        ( do
            putStrLn entry
            S.sendAll sock (C8.pack (entry ++ cCRLF))
        )
        ( \e ->
            do
              let err = show (e :: E.IOException)
              hPutStrLn stderr ("--> Error (listFiles): " ++ err)
              E.throw e
        )

-- cmdStru handles the STRU command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
cmdStru :: Socket -> UserState -> [String] -> IO UserState
cmdStru sock state params
  | getIsLoggedin state =
    if checkParams params 1
      then case structure of
        "F" -> do
          sendLine sock "200 Structure set to F."
          return state
        _ -> do
          sendLine sock "504 Command not implemented for that parameter."
          return state
      else do
        sendLine sock "501 Syntax error in parameters or arguments."
        return state
  where
    structure = strToUpper (getFirst params)

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
              sendLine sock "550 Requested action not taken. Not Allowed."
              return state
          | otherwise ->
            do
              let transType = getType state
              E.catch
                ( do
                    let filePath = getFirst params
                    path <- canonicalizePath filePath
                    if isSubdirectory (getRootDir state) (getParentDirectory path)
                      then do
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
                      else do
                        sendLine sock "550 Requested action not taken. Not Allowed."
                        return state
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

-- cmdCdup handles the CDUP command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
cmdCdup :: Socket -> UserState -> [String] -> IO UserState
cmdCdup sock state params
  | getIsLoggedin state = do
    if 'e' `elem` getPerms state
      then do
        if checkParams params 0
          then do
            rootDir <- makeAbsolute (getRootDir state)
            currDir <- getCurrentDirectory
            if isSubdirectory rootDir currDir && not (equalFilePath rootDir currDir)
              then do
                let parentDir = getParentDirectory currDir
                setCurrentDirectory parentDir
                sendLine sock ("200 Command okay. Current Directory: " ++ parentDir)
                return state
              else do
                sendLine sock "550 Requested action not taken. No Access."
                return state
          else do
            sendLine sock "501 Syntax error in parameters or arguments."
            return state
      else do
        sendLine sock "550 Requested action not taken. Not Allowed."
        return state
  | otherwise = do
    sendLine sock "530 Not logged in."
    return state

-- cmdCwd handles the CDUP command
-- takes: controlSocket, UserState, and a parameter list provided by the client.
cmdCwd :: Socket -> UserState -> [String] -> IO UserState
cmdCwd sock state params
  | getIsLoggedin state = do
    if 'e' `elem` getPerms state
      then do
        if checkParams params 1
          then do
            cwdHelper sock state userPath
          else do
            sendLine sock "501 Syntax error in parameters or arguments."
            return state
      else do
        sendLine sock "550 Requested action not taken. Not Allowed."
        return state
  | otherwise = do
    sendLine sock "530 Not logged in."
    return state
  where
    userPath = getFirst params

-- cwdHelper helper function for cwd
-- takes: controlSocket, UserState, requested path.
cwdHelper :: Socket -> UserState -> FilePath -> IO UserState
cwdHelper sock state path = do
  rootDir <- canonicalizePath (getRootDir state)
  absPath <- canonicalizePath path
  validDir <- doesDirectoryExist absPath
  if validDir && isSubdirectory rootDir absPath
    then do
      setCurrentDirectory absPath
      sendLine sock ("250 Requested file action okay. Current Directory: " ++ absPath)
      return state
    else do
      sendLine sock "550 Requested action not taken. Invalid directory or no access."
      return state

-- sendFile sends a file over the data connection; RETR command helper
-- takes: controlSocket, (dataSock, dataSockAddr), and a file handle.
-- Returns true on a successful send, false otherwise.
sendFile :: Socket -> (Socket, SockAddr) -> Handle -> Type -> IO Bool
sendFile controlSock (dataSock, _) fHandle dataType =
  do
    if dataType == ASCII
      then do
        sendASCII controlSock dataSock fHandle
      else do
        sendBinary controlSock dataSock fHandle

-- sendASCII sends file in ASCII format
-- takes: controlSocket, dataSock, and handle to the file
--        that holds the data that will be sent.
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
    bCR = C8.pack "\r"
    bLF = C8.pack "\n"
    replaceEOL :: BS.ByteString -> BS.ByteString
    replaceEOL bs =
      if BS.drop (BS.length bs - 1) bs == bCR -- check if it ends in CR
        then BS.append bs bLF
        else BS.append bs $ BS.append bCR bLF

-- sendBinary sends file in binary format
-- takes: controlSocket, dataSock, and handle to the file
--        that holds the data that will be sent.
sendBinary :: Socket -> Socket -> Handle -> IO Bool
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

-- returns the upper level directory or the root
getParentDirectory :: FilePath -> FilePath
getParentDirectory path = takeDirectory (dropTrailingPathSeparator path)

-- checks if path starts with the same path as the rootDir
-- expects both FilePaths to be absolute
isSubdirectory :: FilePath -> FilePath -> Bool
isSubdirectory rootDir path
  | equalFilePath rootDir path = True
  | length pathDirs >= length rootDirs = foldr ((&&) . (\x -> fst x == snd x)) True (zip rootDirs pathDirs)
  | otherwise = False
  where
    rootDirs = tail (splitDirectories rootDir)
    pathDirs = tail (splitDirectories path)

-- takes a parameter list, and checks if it has the expected number of
-- parameters, which is specified by count
checkParams :: [String] -> Int -> Bool
checkParams params count = length params == count
