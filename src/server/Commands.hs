module Commands where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C8 (pack, unpack)
import Data.Char
import Network.Socket
import qualified Network.Socket.ByteString as S
import SockNetwork
import System.IO

-- username password isLoggedIn permissions rootDir dataSock
data UserState = UserState
  { username :: String,
    password :: String,
    isLoggedIn :: Bool,
    permissions :: String, -- user access permissions
    rootDir :: String, -- starting directory of the user
    dataSock :: DataConnection -- data connection socket
  }
  deriving (Show)

data DataConnection = NoConnection | DataSocket Socket
  deriving (Show)

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
  [ UserState "test" "test" False "" "elrwd" NoConnection,
    UserState "user" "user" False "" "elrwd" NoConnection
  ]

-- executeCommand executes an ftp command and send a reply to the client
executeCommand :: Socket -> UserState -> String -> [String] -> IO UserState
executeCommand sock state command paramLst
  | command == "USER" = do cmdUser sock state paramLst
  | command == "PASS" = do cmdPass sock state paramLst
  | otherwise = do
    sendLine sock "502 Command not implemented"
    return state

-- sendLine sends a line over the control connection to the client
-- Throws an exception if an error occurs while sending
sendLine :: Socket -> String -> IO ()
sendLine sock str =
  E.catch
    ( do
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
              return (UserState "" "" False "" "" NoConnection)
    else do
      sendLine sock "501 Syntax error in parameters or arguments."
      return state
  where
    passwordRecived = getFirst params
    passwordActual = getPass state

-- Takes a username, and returns the inital state of that user that is stored in
-- usersList
-- Assumes that username exists in the list of usersList
getUserState :: String -> IO UserState
getUserState name =
  return
    ( head
        (filter (\(UserState username _ _ _ _ _) -> name == username) usersList)
    )

doesUserExist :: String -> [UserState] -> Bool
doesUserExist user [] = False
doesUserExist user users = (user == username) || doesUserExist user (tail users)
  where
    username = getUsername (head users)

setIsLoggedIn :: Bool -> UserState -> UserState
setIsLoggedIn status (UserState username password _ permissions rootDir dataSock) =
  UserState username password status permissions rootDir dataSock

setDataSock :: DataConnection -> UserState -> UserState
setDataSock dataConn (UserState username password isLoggedIn permissions rootDir _) =
  UserState username password isLoggedIn permissions rootDir dataConn

getUsername :: UserState -> String
getUsername (UserState username _ _ _ _ _) = username

getPass :: UserState -> String
getPass (UserState _ password _ _ _ _) = password

getIsLoggedin :: UserState -> Bool
getIsLoggedin (UserState _ _ isLoggedIn _ _ _) = isLoggedIn

getPerms :: UserState -> String
getPerms (UserState _ _ _ permissions _ _) = permissions

getRootDir :: UserState -> String
getRootDir (UserState _ _ _ _ rootDir _) = rootDir

getDataSock :: UserState -> DataConnection
getDataSock (UserState _ _ _ _ _ dataSock) = dataSock

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