module Network.XMPP.XMPPConnection
    ( XMPPConnection(..)
    ) where

import Data.ByteString (ByteString)

-- | A class for various kinds of XMPP connections.
-- It could be standart TCP connection,
-- HTTP Polling <http://xmpp.org/extensions/xep-0025.html> (obsolete),
-- BOSH <http://xmpp.org/extensions/xep-0124.html>,
-- HTTP/SOCKS proxy connection and so on.
class XMPPConnection c where
    -- | Synchronously get new data from the connection.
    getBytes :: c -> IO ByteString
    -- | Send data over the connection.
    sendBytes :: c -> ByteString -> IO ()
    -- | Close the connection.
    closeConnection :: c -> IO ()
