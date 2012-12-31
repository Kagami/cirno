-- | Implementation of the RFC 6122 (XMPP ADDR)
-- <http://xmpp.org/rfcs/rfc6122.html>.

module Network.XMPP.JID
    ( JID(..)
    , bareJID
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Network.XMPP.Utils (ReadT(..), ShowT(..))

-- | An XMPP entity is anything that is network-addressable and that
-- can communicate using XMPP. For historical reasons, the native
-- address of an XMPP entity is called a Jabber Identifier or JID.
data JID = JID
    { -- | The localpart of a 'JID' is an optional identifier placed
      -- before the 'domainpart' and separated from the latter by the
      -- \'@\' character.
      localpart :: Maybe Text

      -- | The domainpart of a 'JID' is that portion after the \'@\'
      -- character (if any) and before the \'/\' character (if any);
      -- it is the primary identifier and is the only REQUIRED element
      -- of a 'JID' (a mere domainpart is a valid 'JID').
    , domainpart :: Text

      -- | The resourcepart of a 'JID' is an optional identifier placed
      -- after the 'domainpart' and separated from the latter by the \'/\'
      -- character.
    , resourcepart :: Maybe Text
    }

-- | Parse 'JID' from 'Text'.
--
-- TODO: stringprep, validation.
instance ReadT JID where
    readT text = JID username server resource
      where
        (username, rest) = case T.findIndex (=='@') text of
            Just n -> (Just $ T.take n text, T.drop (n+1) text)
            _      -> (Nothing, text)
        (server, resource) = case T.findIndex (=='/') rest of
            Just n -> (T.take n rest, Just $ T.drop (n+1) rest)
            _      -> (rest, Nothing)

-- | Very basic 'Read' instance. Doesn't support complex or nested types.
instance Read JID where
    readsPrec _ s = [(readT $ T.pack s, "")]

-- | Show full JID.
instance ShowT JID where
    showT JID { .. } =
        maybe "" (T.cons '@') localpart <>
        domainpart <>
        maybe "" (T.cons '/') resourcepart

-- | Show full JID.
instance Show JID where
    show = T.unpack . showT

-- | Drop resourcepart of the given JID.
bareJID :: JID -> JID
bareJID JID { .. } = JID localpart domainpart Nothing
