module SockNetwork where
 
import Network.Socket


-- Create a socket and binds it to the specified port
-- if port is 0, then it will bind to any avaliable port 
createSock :: String -> IO Socket
createSock port = do
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE], addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1 -- make socket immediately reusable - eases debugging.
  bind sock (addrAddress addr)
  listen sock 1 -- only listen on one client
  return sock

-- getAddrPort takes a socketAddr and return the address and port
-- this socket is bound to in PASV response format: (h1,h2,h3,h4,p1,p2).
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
