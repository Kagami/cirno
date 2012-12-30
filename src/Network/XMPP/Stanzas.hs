module Network.XMPP.Stanzas
    (
      -- * Helpers
      genResource
    , genStanzaId
    , getBody
      -- * Predicates
    , (&)
    , hasNodeName
    , attributeMatches
    , attributeEquals
    , idMatches
    , isMessage
    , isChat
    , isFrom
    , isFromBare
      -- * Manage stanzas
    , sendIq
    , sendIqWait
    , sendInitialPresence
    , sendMessage
    ) where

import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Unique (newUnique, hashUnique)
import System.Random (getStdGen, randomRs)
import qualified Data.Text as T

import Network.XMPP.JID (bareJID)
import Network.XMPP.XML (XML(..), xmlPath, getCData)
import Network.XMPP.Monad (XMPP, StanzaPredicate, sendStanza, waitForStanza)
import Network.XMPP.Utils (readT, showT)

--------------------------------------------------
-- Stanza helpers.
--------------------------------------------------

-- | Generate random resource for the connection.
genResource :: IO Text
genResource = do
    g <- getStdGen
    return $ T.pack $ take 10 $ randomRs ('a', 'z') g

-- | Generate unique ID for stanza.
-- Note that:
-- > Two Uniques may hash to the same value, although in practice
-- > this is unlikely. The Int returned makes a good hash key.
genStanzaId :: IO Text
genStanzaId = newUnique >>= return . showT . hashUnique

-- | Try to get body from the given stanza.
getBody :: XML -> Maybe Text
getBody el = xmlPath ["body"] el >>= getCData

--------------------------------------------------
-- Stanza predicates.
--------------------------------------------------

-- | Conjunction of two predicates.
(&) :: StanzaPredicate -> StanzaPredicate -> StanzaPredicate
pred1 & pred2 = \stanza -> pred1 stanza && pred2 stanza

-- | Return 'True' if the stanza has the given node name.
hasNodeName :: Text -> StanzaPredicate
hasNodeName name (XML name' _ _) = name == name'
hasNodeName _    _               = False

-- | Apply the predicate to the named attribute. Return 'False' if the
-- tag has no such attribute.
attributeMatches :: Text            -- ^ Attribute name
                 -> (Text -> Bool)  -- ^ Attribute value predicate
                 -> StanzaPredicate
attributeMatches attr predicate (XML _ attrs _) =
    maybe False predicate (lookup attr attrs)
attributeMatches _ _ _ =
    False

-- | The same as 'attributeMatches' but always check for equality.
attributeEquals :: Text             -- ^ Attribute name
                -> Text             -- ^ Attribute value
                -> StanzaPredicate
attributeEquals attr value = attributeMatches attr (==value)

-- | Return 'True' if the stanza\'s id equals to the given.
idMatches :: Text -> StanzaPredicate
idMatches stanzaId = attributeEquals "id" stanzaId

-- | Return 'True' if the stanza is a message stanza.
isMessage :: StanzaPredicate
isMessage = hasNodeName "message"

-- | Return 'True' if the stanza is a chat message.
isChat :: StanzaPredicate
isChat = isMessage & attributeEquals "type" "chat"

-- |Return 'True' if the stanza is from the given JID.
isFrom :: Text -> StanzaPredicate
isFrom = attributeEquals "from"

-- |Return 'True' if the stanza\'s bare JID equals to the given.
isFromBare :: Text -> StanzaPredicate
isFromBare jid =
    attributeMatches "from" (\jid' -> (bareJID $ readT jid') == jid)

--------------------------------------------------
-- Constructors for common stanzas.
--------------------------------------------------

--------------------------------------------------
-- Manage stanzas.
--------------------------------------------------

-- | Send an IQ request, return the randomly generated ID.
sendIq :: Text       -- ^JID of recipient
       -> Text       -- ^Type of IQ, either \"get\" or \"set\"
       -> [XML]      -- ^Payload elements
       -> XMPP Text  -- ^ID of sent stanza
sendIq jidTo iqType payload = do
    iqId <- liftIO genStanzaId
    sendStanza $ XML "iq"
                     [ ("to", jidTo)
                     , ("type", iqType)
                     , ("id", iqId)
                     ] payload
    return iqId

-- | Send an IQ request and wait for the response, without blocking
-- other activity.
sendIqWait :: Text     -- ^JID of recipient
           -> Text     -- ^Type of IQ, either \"get\" or \"set\"
           -> [XML]    -- ^Payload elements
           -> XMPP XML -- ^Response stanza
sendIqWait jidTo iqType payload = do
    iqId <- sendIq jidTo iqType payload
    waitForStanza $ idMatches iqId

-- | Send ordinary initial presence.
-- See <http://xmpp.org/rfcs/rfc6121.html#presence-initial>.
sendInitialPresence :: XMPP ()
sendInitialPresence = sendStanza $ XML "presence" [] []

-- | Send an ordinary \"chat\" type message.
sendMessage :: Text -- ^JID of recipient
            -> Text -- ^Text of message
            -> XMPP ()
sendMessage jidTo body =
    sendStanza $ XML "message"
                     [ ("to", jidTo)
                     , ("type", "chat")
                     ]
                     [XML "body" []
                        [CData body]]
