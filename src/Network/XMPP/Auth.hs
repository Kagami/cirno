module Network.XMPP.Auth
    ( legacyAuth
    ) where

import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as T

import Network.XMPP.JID (JID(..))
import Network.XMPP.XML (XML(..))
import Network.XMPP.Monad (XMPP, getJID)
import Network.XMPP.Stanzas (genResource, sendIqWait)

-- | Implement XEP-0078: Non-SASL Authentication
-- FIXME: Check for errors.
legacyAuth :: Text     -- ^ Password
           -> XMPP ()
legacyAuth password = do
    jid <- getJID
    resource <- if T.null $ jidResource jid
                    then liftIO $ genResource
                    else return $ jidResource jid
    _response <- sendIqWait (jidServer jid) "set"
        [XML "query"
             [("xmlns", "jabber:iq:auth")]
             [ XML "username" [] [CData $ jidUsername jid]
             , XML "password" [] [CData password]
             , XML "resource" [] [CData resource]
             ]]
    return ()
