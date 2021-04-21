import qualified Network.Socket as N
import System.IO

-- Configuration options
myServer = "irc.freenode.org" :: String

myPort = 6667 :: N.PortNumber

main :: IO ()
main = do
  h <- connectTo myServer myPort
  t <- hGetContents h
  hSetBuffering stdout NoBuffering
  print t

-- Connect to a server given its name and port
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode