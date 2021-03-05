module UserState where

import Network.Socket (Socket)
import System.FilePath

data DataConnection = NoConnection | DataSocket Socket
  deriving (Show, Eq)

-- Transfer type
data Type = ASCII | Binary
  deriving (Show, Eq)

-- Data structure that holds info about the user state
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

-- A list of users who are allowed to access the ftp server
usersList :: [UserState]
usersList =
  [ UserState "test" "test" False "elrwd" "" NoConnection ASCII,
    UserState "user" "user" False "elrwd" "" NoConnection ASCII
  ]

-- Takes a username, and returns the inital state of that user that is stored in usersList
-- Assumes that the username exists in the list of usersList
getUserState :: String -> IO UserState
getUserState name =
  return
    ( head
        (filter (\(UserState username _ _ _ _ _ _) -> name == username) usersList)
    )

-- Checks if a user exists in the list of users
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
setTransType transType (UserState username password isLoggedIn permissions rootDir dataSock _) =
  UserState username password isLoggedIn permissions rootDir dataSock transType

-- Sets the rootDir to dir only if rootDir is empty
-- in most cases, dir will be the directory where the server is running
setRootDir :: FilePath -> UserState -> UserState
setRootDir dir (UserState username password isLoggedIn permissions rootDir dataSock transType)
  | equalFilePath rootDir "" = UserState username password isLoggedIn permissions dir dataSock transType
  | otherwise = UserState username password isLoggedIn permissions rootDir dataSock transType

getUsername :: UserState -> String
getUsername (UserState username _ _ _ _ _ _) = username

getPass :: UserState -> String
getPass (UserState _ password _ _ _ _ _) = password

getIsLoggedin :: UserState -> Bool
getIsLoggedin (UserState _ _ isLoggedIn _ _ _ _) = isLoggedIn

getPerms :: UserState -> String
getPerms (UserState _ _ _ permissions _ _ _) = permissions

getRootDir :: UserState -> FilePath
getRootDir (UserState _ _ _ _ rootDir _ _) = rootDir

getDataSock :: UserState -> DataConnection
getDataSock (UserState _ _ _ _ _ dataSock _) = dataSock

getType :: UserState -> Type
getType (UserState _ _ _ _ _ _ transferType) = transferType
