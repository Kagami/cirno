module Network.XMPP.TCPConnection
    ( TCPConnection
    , openTCPConnection
    ) where

import Control.Applicative ((<$>))
import Data.Text (Text)
import Network.Socket (Socket, Family(AF_INET6), SocketType(Stream),
                       defaultProtocol, getAddrInfo, addrAddress,
                       socket, connect, close)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.Text as T

import Network.XMPP.Monad (XMPPState, initXMPP)
import Network.XMPP.XMPPConnection (XMPPConnection(..))

-- | An XMPP connection over TCP.
newtype TCPConnection = TCPConnection Socket

type Server = Text
type Port = Int

-- | Open a TCP connection to the named server and send a stream header.
openTCPConnection :: Server -> Maybe Port -> IO XMPPState
openTCPConnection server mport = do
    sock <- socket AF_INET6 Stream defaultProtocol
    let host = T.unpack server
    let port = maybe "5222" show mport
    addrInfo <- head <$> getAddrInfo Nothing (Just host) (Just port)
    connect sock (addrAddress addrInfo)
    initXMPP $ TCPConnection sock

instance XMPPConnection TCPConnection where
    getBytes (TCPConnection sock) =
        recv sock 4096
    sendBytes (TCPConnection sock) =
        sendAll sock
    closeConnection (TCPConnection sock) =
        close sock
