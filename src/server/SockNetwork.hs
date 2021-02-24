module SockNetwork where
 
import Network.Socket
import Network.Info
import Data.Maybe


-- Create a socket and binds it to the specified port.
-- If port is 0, then it will bind to any avaliable port 
createSock :: String -> IO Socket
createSock port = do
  let hints = defaultHints {addrFamily = AF_INET, addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE], addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1 -- make socket immediately reusable.
  bind sock (addrAddress addr)
  listen sock 1 -- only listen on one client
  return sock

-- getAddrPort takes a socket and returns the address and port
-- this socket is bound to in PASV response format: (h1,h2,h3,h4,p1,p2).
getAddrPort :: Socket -> IO String
getAddrPort socket  =
  do
    sockAddr <- getSocketName socket
    nameInfo <- getNameInfo [NI_NUMERICSERV] False True sockAddr
    ipStr <- getIPAddr

    let host = formatIP ipStr
        port = fromJust $ snd nameInfo
        portNum = read port  :: Integer
        ports = formatResponse [portNum `div` 256, portNum `mod` 256]
        npa = "(" ++ host ++ "," ++ init ports ++ ")."

    putStrLn ("--> " ++ npa) -- DEBUG Info
    return npa
  where
    formatResponse lst = foldr (\x y -> show x ++ "," ++ y) [] lst
    formatIP str = map replace str


-- Gets the ip address of the first non-loopback network interface
-- that the server is listening on.
getIPAddr :: IO String
getIPAddr =
  do
    interfaces <- getNetworkInterfaces
    return (foldr (\(NetworkInterface name ipv4 _ _ ) y -> if (show ipv4) `notElem` loInterface then show ipv4 else y) [] interfaces)
  where
    loInterface = ["127.0.0.1", "0.0.0.0"]

-- replaces a character with another
replace :: Char -> Char
replace c
  | c == '.' = ','
  | otherwise = c