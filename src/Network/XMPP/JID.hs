-- | Implementation of the RFC 6122 (XMPP ADDR)
-- <http://xmpp.org/rfcs/rfc6122.html>

module Network.XMPP.JID
    ( JID(..)
    , bareJid
    , fullJid
    ) where

import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Network.XMPP.Utils (ReadT(..))

data JID = JID
    { jidUsername :: Text
    , jidServer   :: Text
    , jidResource :: Text
    }

-- | Very basic 'Read' instance. Doesn't support complex or nested types.
instance Read JID where
    readsPrec _ s = [(readT $ T.pack s, "")]

-- | Parse 'JID' from 'Text'.
-- TODO: stringprep, validation.
instance ReadT JID where
    readT text = JID username server resource
      where
        (username, rest) = case T.findIndex (=='@') text of
            Just n -> (T.take n text, T.drop (n+1) text)
            _      -> ("", text)
        (server, resource) = case T.findIndex (=='/') rest of
            Just n -> (T.take n rest, T.drop (n+1) rest)
            _      -> (rest, "")

bareJid :: JID -> Text
bareJid JID { .. } =
    if T.null jidUsername
        then jidServer
        else jidUsername <> "@" <> jidServer

fullJid :: JID -> Text
fullJid jid@JID { .. } =
    if T.null jidResource
        then bareJid jid
        else bareJid jid <> "/" <> jidResource
