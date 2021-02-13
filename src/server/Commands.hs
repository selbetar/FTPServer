module Commands where

import Data.Char
import Network.Socket
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
executeCommand :: Handle -> UserState -> String -> [String] -> IO UserState
executeCommand controlHdl state command paramLst
  | command == "USER" = do cmdUser controlHdl state paramLst
  | command == "PASS" = do cmdPass controlHdl state paramLst
  | otherwise = do
    sendLine controlHdl "502 Command not implemented"
    return state

-- sendLine sends a line over the control connection to the client
sendLine :: Handle -> [Char] -> IO ()
sendLine controlHdl line = do
  putStrLn (">> " ++ line)
  hPutStr controlHdl (line ++ cCRLF)
  hFlush controlHdl

cmdUser :: Handle -> UserState -> [String] -> IO UserState
cmdUser controlHdl state params =
  if checkParams params 1
    then
      if doesUserExist username usersList
        then do
          -- update the state
          uState <- getUserState username
          sendLine controlHdl "331 Username OK. Send password."
          return uState
        else do
          sendLine controlHdl "530 User doesn't exist"
          return state
    else do
      sendLine controlHdl "501 Syntax error in parameters or arguments."
      return state
  where
    username = getFirst params

cmdPass :: Handle -> UserState -> [String] -> IO UserState
cmdPass controlHdl state params =
  if checkParams params 1
    then
      if getUsername state == ""
        then do
          sendLine controlHdl "503 Login with a user first."
          return state
        else
          if passwordActual == passwordRecived
            then do
              sendLine controlHdl "230 User logged in, proceed."
              return (setIsLoggedIn True state)
            else do
              sendLine controlHdl "530 Incorrect password"
              return (UserState "" "" False "" "" NoConnection)
    else do
      sendLine controlHdl "501 Syntax error in parameters or arguments."
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
isCmdValid cmd = (strToUpper cmd) `elem` commands

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