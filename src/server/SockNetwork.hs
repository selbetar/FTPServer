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
