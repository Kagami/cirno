module Network.XMPP.TCPConnection
    ( TCPConnection
    , openTCPConnection
    ) where

import Control.Applicative ((<$>))
import Network.Socket (Socket, Family(AF_INET), SocketType(Stream),
                       defaultProtocol, getAddrInfo, addrAddress,
                       socket, connect, close)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.Text as T

#ifdef DEBUG
import Data.Monoid ((<>))
import Debug.Trace (traceIO)
import qualified Data.ByteString.Char8 as S
#endif

import Network.XMPP.JID (JID(jidServer))
import Network.XMPP.Monad (XMPPState, initXMPP)
import Network.XMPP.XMPPConnection (XMPPConnection(..))

-- FIXME: check for errors!

-- | An XMPP connection over TCP.
newtype TCPConnection = TCPConnection Socket

-- | Open a TCP connection to the named server and send a stream header.
-- FIXME: Address family must be AF_INET6:
-- > If AF_INET6 is used, the IPv6Only socket option is set to 0 so
-- > that both IPv4 and IPv6 can be handled with one socket
-- But in fact that doesn't work:
-- > info <- head `fmap` getAddrInfo Nothing (Just "localhost") (Just "5222")
-- > sock <- socket AF_INET6 Stream defaultProtocol
-- > connect sock (addrAddress info)
-- *** Exception: connect: invalid argument (Invalid argument)
openTCPConnection :: JID -> Maybe Int -> IO XMPPState
openTCPConnection jid mport = do
    sock <- socket AF_INET Stream defaultProtocol
    let host = T.unpack $ jidServer jid
    let port = maybe "5222" show mport
    addrInfo <- head <$> getAddrInfo Nothing (Just host) (Just port)
    connect sock (addrAddress addrInfo)
    initXMPP (TCPConnection sock) jid

instance XMPPConnection TCPConnection where

    getBytes (TCPConnection sock) = do
#ifdef DEBUG
        bytes <- recv sock 4096
        traceIO $ S.unpack $ "\ESC[1;31m" <> "GET: " <> "\ESC[0m" <> bytes
        return bytes
#else
        recv sock 4096
#endif

    sendBytes (TCPConnection sock) bytes = do
#ifdef DEBUG
        traceIO $ S.unpack $ "\ESC[1;31m" <> "SEND: " <> "\ESC[0m" <> bytes
        sendAll sock bytes
#else
        sendAll sock bytes
#endif

    closeConnection (TCPConnection sock) =
        close sock
