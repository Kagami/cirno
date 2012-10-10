module Network.XMPP.XMPPConnection
    ( XMPPConnection(..)
    ) where

import Network.XMPP.XML (XMLElem)

-- | A class for various kinds of XMPP connections.
-- It could be standart TCP connection,
-- HTTP Polling <http://xmpp.org/extensions/xep-0025.html> (obsolete),
-- BOSH <http://xmpp.org/extensions/xep-0124.html>,
-- HTTP/SOCKS proxy connection and so on.
class XMPPConnection c where
    -- | Get incoming stanzas from the connection.
    getStanzas :: c -> IO [XMLElem]
    -- | Send a stanza on the connection.
    sendStanza :: c -> XMLElem -> IO ()
    -- | Close the connection.
    closeConnection :: c -> IO ()
