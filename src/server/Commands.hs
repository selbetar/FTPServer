module Commands where

import Data.Char
import Network.Socket
import SockNetwork
import System.IO

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
cCRLF = "\n\r"

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

users :: [UserState]
users =
  [ UserState "test" "test" False "" "elrwd" NoConnection,
    UserState "user" "" False "" "elrwd" NoConnection
  ]

-- executeCommand executes an ftp command and send a reply to the client
executeCommand :: Handle -> UserState -> String -> [String]-> IO UserState
executeCommand controlHdl state command paramLst =
  do
    -- TODO
    sendLine controlHdl "530"
    return state

-- sendLine sends a line over the control connection to the client
sendLine :: Handle -> [Char] -> IO ()
sendLine controlHdl line = do
  putStrLn (">> " ++ line)
  hPutStrLn controlHdl (line ++ cCRLF)

--isUserPassValid
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
splitsep f lst = split lst []
  where
    split [] [] = [[]]
    split [] lst = [lst]
    split (h : t) lst
      | f h = lst : split t []
      | otherwise = split t (lst ++ [h])