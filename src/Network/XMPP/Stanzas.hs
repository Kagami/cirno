module Network.XMPP.Stanzas
    ( initStream
    ) where

import Network.XMPP.Monad (XMPP, sendStreamStart)

-- | Initialize XMPP stream. Must be run just after the connection
-- was established.
initStream :: XMPP ()
initStream = do
    sendStreamStart
