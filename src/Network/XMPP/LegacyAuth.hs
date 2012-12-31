module Network.XMPP.LegacyAuth
    ( legacyAuth
    ) where

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)

import Network.XMPP.JID (JID(..))
import Network.XMPP.XML (XML(..))
import Network.XMPP.Monad (XMPP, getJID)
import Network.XMPP.Stanzas (genResource, sendIqWait)

-- | Implement XEP-0078: Non-SASL Authentication
--
-- FIXME: Check for errors.
legacyAuth :: Text     -- ^ Password
           -> XMPP ()
legacyAuth password = do
    JID { .. } <- getJID
    resource <- maybe (liftIO genResource) return resourcepart
    _response <- sendIqWait domainpart "set"
        [XML "query"
             [("xmlns", "jabber:iq:auth")]
             [ XML "username" [] [CData $ fromJust localpart]
             , XML "password" [] [CData password]
             , XML "resource" [] [CData resource]
             ]]
    return ()
