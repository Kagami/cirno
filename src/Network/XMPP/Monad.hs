module Network.XMPP.Monad
    ( XMPP
    , initXMPP
    , getJID
    , runXMPP
    , runXMPPLoop
    , XMPPState
    , StanzaPredicate
    , StanzaHandler
    , addHandler
    , addHandlerOnce
    , waitForStanza
    , sendStanza
    , initStream
    ) where

import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString (ByteString)
import Data.Unique (Unique, newUnique)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO,
                               modifyTVar', newEmptyTMVarIO, takeTMVar,
                               putTMVar)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as S

import Network.XMPP.JID (JID(..))
import Network.XMPP.XML (XML(..), XMLParser, xml2bytes, tag2bytes,
                         parseXML, parseStreamStart)
import Network.XMPP.XMPPConnection (XMPPConnection(getBytes, sendBytes))

-- | Stanza handler (callback).
data XMPPHandler = Catch StanzaPredicate StanzaHandler
                 | CatchOnce StanzaPredicate StanzaHandler

type StanzaPredicate = XML -> Bool
type StanzaHandler = XML -> XMPP ()

-- | Handlers dict. Each handler has assotiated unique value with him
-- (created on handler addition).
type XMPPHandlers = M.Map Unique XMPPHandler

-- | State which we can read and modify inside monad.
data XMPPState = forall c. XMPPConnection c => XMPPState
    { stateConnection :: c
    , stateBuffer :: IORef Text
    , stateHandlers :: TVar XMPPHandlers
    , stateJID :: JID
    }

-- | XMPP monad.
newtype XMPP a = XMPP { unXMPP :: ReaderT XMPPState IO a }
    deriving (Functor, Monad, MonadIO)

-- | Initialize monad state.
initXMPP :: XMPPConnection c => c -> JID -> IO XMPPState
initXMPP c jid = do
    bufvar <- newIORef T.empty
    handlers <- newTVarIO M.empty
    return $ XMPPState c bufvar handlers jid

-- | Get user's JID from the state.
getJID :: XMPP JID
getJID = do
    XMPPState { stateJID } <- XMPP ask
    return stateJID

-- | Run function inside the XMPP monad.
runXMPP :: XMPPState -> XMPP a -> IO a
runXMPP state m = runReaderT (unXMPP m) state

-- FIXME: We are needed in this function because I don't know
-- how to make 'waitForStanza' work in async style.
runXMPPForked :: XMPPState -> XMPP () -> IO ()
runXMPPForked state = void . forkIO . runXMPP state

-- | Run function inside the XMPP monad. After that, keep looping.
-- Note what this function must be called only once in the main thread.
-- Use 'runXMPP' with the same state in other threads for working with
-- XMPP (e.g. start worker thread and send message when it's done).
runXMPPLoop :: XMPPState -> XMPP () -> IO ()
runXMPPLoop state m = do
    runXMPPForked state m
    runXMPPLoop' state []

-- FIXME: we will loop forever.
runXMPPLoop' :: XMPPState -> [XML] -> IO ()
runXMPPLoop' state [] =
    runXMPP state (getStanzas parseXML) >>= runXMPPLoop' state
runXMPPLoop' state@(XMPPState { .. }) (stanza:stanzas) = do
    handlers <- readTVarIO stateHandlers
    case findHandler stanza handlers of
        Just (_, Catch _ stanzaHandler) ->
            runXMPPForked state (stanzaHandler stanza)
        Just (uniq, CatchOnce _ stanzaHandler) -> do
            atomically $ modifyTVar' stateHandlers (M.delete uniq)
            runXMPPForked state (stanzaHandler stanza)
        Nothing ->
            return ()
    runXMPPLoop' state stanzas

-- | Find handler for the given stanza using predicates.
findHandler :: XML -> XMPPHandlers -> Maybe (Unique, XMPPHandler)
findHandler stanza = findHandler' stanza . M.toList

findHandler' :: XML -> [(Unique, XMPPHandler)]
             -> Maybe (Unique, XMPPHandler)
findHandler' _ [] = Nothing
findHandler' stanza ((uniq, handler):handlers) =
    if predicate stanza
       then Just (uniq, handler)
       else findHandler' stanza handlers
  where
    predicate = case handler of
        Catch p _ -> p
        CatchOnce p _ -> p

-- | Add handler that will be executed each time when the
-- given predicate matches stanza.
addHandler :: StanzaPredicate -> StanzaHandler -> XMPP ()
addHandler = (addHandler' .) . Catch

-- | The same as 'addHandler' but will be executed only once.
addHandlerOnce :: StanzaPredicate -> StanzaHandler -> XMPP ()
addHandlerOnce = (addHandler' .) . CatchOnce

addHandler' :: XMPPHandler -> XMPP ()
addHandler' handler = do
    XMPPState { .. } <- XMPP ask
    uniq <- liftIO $ newUnique
    liftIO $ atomically $ modifyTVar' stateHandlers (M.insert uniq handler)

-- | Blockingly get new stanzas from the connection.
getStanzas :: XMLParser -> XMPP [XML]
getStanzas = getStanzas' S.empty

getStanzas' :: ByteString -> XMLParser -> XMPP [XML]
getStanzas' cache parser = do
    XMPPState { .. } <- XMPP ask
    input <- liftIO $ getBytes stateConnection
    case decodeUtf8' input of
        Left _ ->
            -- XXX: UTF-8 decoding error. Add data to cache and try once
            -- more. It could be a problem if we will often get
            -- not-valid chunks of UTF-8 data from socket.
            getStanzas' (cache <> input) parser
        Right input' -> do
            buffer <- liftIO $ readIORef stateBuffer
            let (tags, rest) = parser (buffer <> input')
            liftIO $ writeIORef stateBuffer rest
            return tags

-- | Blockingly wait for the stanza matching the given predicate.
waitForStanza :: StanzaPredicate -> XMPP XML
waitForStanza predicate = do
    box <- liftIO newEmptyTMVarIO
    addHandlerOnce predicate $ liftIO . atomically . (putTMVar box)
    liftIO $ atomically $ takeTMVar box

-- | Send given stanza over the current XMPP connection.
sendStanza :: XML -> XMPP ()
sendStanza stanza = do
    XMPPState { .. } <- XMPP ask
    liftIO $ sendBytes stateConnection $ xml2bytes stanza

-- | Initialize XMPP stream. Must be run just after the connection
-- was established.
-- FIXME: We lose stream features stanza.
initStream :: XMPP ()
initStream = do
    XMPPState { .. } <- XMPP ask
    liftIO $ sendBytes stateConnection $ streamStart stateJID
    _stream <- head <$> getStanzas parseStreamStart
    return ()

-- | Opening stream tag for the given XMPP server.
streamStart :: JID -> ByteString
streamStart JID { .. } =
    "<?xml version='1.0'?>" <>
    (tag2bytes $
        XML "stream:stream"
            [ ("to", domainpart)
            , ("xmlns", "jabber:client")
            , ("xmlns:stream","http://etherx.jabber.org/streams")
            ]
        [])
