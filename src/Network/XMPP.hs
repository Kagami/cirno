module Network.XMPP
    ( (<>)
    -- * XML functions
    , XML(..)
    -- * JID functions
    , module Network.XMPP.JID
    -- * The XMPP monad
    , module Network.XMPP.Monad
    -- * Stanza-related functional
    , module Network.XMPP.Stanzas
    -- * Helpers
    , module Network.XMPP.Utils
    -- * Manage connections
    , openTCPConnection
    ) where

import Data.Monoid ((<>))

import Network.XMPP.JID
import Network.XMPP.Monad hiding (initXMPP, sendStreamStart)
import Network.XMPP.XML
import Network.XMPP.Stanzas
import Network.XMPP.Utils
import Network.XMPP.TCPConnection (openTCPConnection)
