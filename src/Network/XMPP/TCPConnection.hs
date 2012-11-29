module Network.XMPP.TCPConnection
    ( TCPConnection
    , openTCPConnection
    ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (Socket, Family(AF_INET6), SocketType(Stream),
                       defaultProtocol, getAddrInfo, addrAddress,
                       socket, connect, close)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.Text as T
import qualified Data.ByteString as S

import Network.XMPP.Monad (XMPPState, initXMPP)
import Network.XMPP.XMPPConnection (XMPPConnection(..))
import Network.XMPP.XML (parseTags, xml2bytes)

-- | An XMPP connection over TCP.
data TCPConnection = TCPConnection Socket (IORef ByteString)

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
    bufvar <- newIORef S.empty
    initXMPP $ TCPConnection sock bufvar

instance XMPPConnection TCPConnection where
    getStanzas (TCPConnection sock bufvar) = do
        buffer <- readIORef bufvar
        input <- recv sock 4096
        let buffer' = buffer <> input
        let (tags, rest) = parseTags buffer'
        writeIORef bufvar rest
        return tags
    sendStanza (TCPConnection sock _) =
        sendAll sock . xml2bytes
    closeConnection (TCPConnection sock _) =
        close sock
