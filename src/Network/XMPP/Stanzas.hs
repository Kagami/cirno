module Network.XMPP.Stanzas
    ( waitForStanza
    , initStream
    ) where

import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO,
                               takeTMVar, putTMVar)
import Data.Text (Text)

import Network.XMPP.Monad (XMPP, StanzaPredicate, addHandlerOnce, sendStanza)
import Network.XMPP.XML (XML(..))

-- | Blockingly wait for the stanza matching the given predicate.
waitForStanza :: StanzaPredicate -> XMPP XML
waitForStanza predicate = do
    box <- liftIO newEmptyTMVarIO
    addHandlerOnce predicate $ liftIO . atomically . (putTMVar box)
    liftIO $ atomically $ takeTMVar box

-- | Get the opening stream string for the given XMPP server.
openStream :: Text -> XML
openStream server =
    XML "stream:stream"
        [ ("to", server)
        , ("xmlns", "jabber:client")
        , ("xmlns:stream","http://etherx.jabber.org/streams")
        ]
        []

-- | Initialize XMPP stream. Must be run just after the connection
-- was established.
initStream :: Text -> XMPP ()
initStream server = do
    sendStanza $ openStream server
