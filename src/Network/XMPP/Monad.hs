module Network.XMPP.Monad
    ( XMPP
    , initXMPP
    , runXMPP
    , runXMPPLoop
    , XMPPState
    , StanzaPredicate
    , StanzaHandler
    , addHandler
    , addHandlerOnce
    ) where

import Data.Unique (Unique, newUnique)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO,
                               modifyTVar')
import qualified Data.Map as M

import Network.XMPP.XML (XMLElem)
import Network.XMPP.XMPPConnection (XMPPConnection, getStanzas)

-- | Stanza handler (callback).
data XMPPHandler = Catch StanzaPredicate StanzaHandler
                 | CatchOnce StanzaPredicate StanzaHandler

type StanzaPredicate = XMLElem -> Bool
type StanzaHandler = XMLElem -> XMPP ()

-- | Handlers dict. Each handler has assotiated unique value with him
-- (created on handler addition).
type XMPPHandlers = M.Map Unique XMPPHandler

-- | State which we can read and modify inside monad.
data XMPPState = forall c. XMPPConnection c => XMPPState
    { stateConnection :: c
    , stateHandlers :: TVar XMPPHandlers
    }

-- | XMPP monad.
newtype XMPP a = XMPP { unXMPP :: ReaderT XMPPState IO a }
    deriving (Monad, MonadIO)

-- | Initialize monad state.
initXMPP :: XMPPConnection c => c -> IO XMPPState
initXMPP c = do
    handlers <- newTVarIO M.empty
    return $ XMPPState c handlers

-- | Run function inside the XMPP monad.
runXMPP :: XMPPState -> XMPP () -> IO ()
runXMPP state m = runReaderT (unXMPP m) state

-- | Run function inside the XMPP monad. After that, keep looping as
-- long as there are handlers left waiting for incoming stanzas from
-- XMPP connection.
-- Note what this function must be called only once in the main thread.
-- Use 'runXMPP' with the same state in other threads for working with
-- XMPP (e.g. start worker thread and send message when it's done).
runXMPPLoop :: XMPPState -> XMPP () -> IO ()
runXMPPLoop state m = do
    runXMPP state m
    runXMPPLoop' state []

runXMPPLoop' :: XMPPState -> [XMLElem] -> IO ()
runXMPPLoop' state@(XMPPState { .. }) [] = do
    handlers <- readTVarIO stateHandlers
    when (not $ M.null handlers) $
        getStanzas stateConnection >>= runXMPPLoop' state
runXMPPLoop' state@(XMPPState { .. }) (stanza:stanzas) = do
    handlers <- readTVarIO stateHandlers
    case findHandler stanza handlers of
        Just (_, Catch _ stanzaHandler) ->
            runXMPP state (stanzaHandler stanza)
        Just (uniq, CatchOnce _ stanzaHandler) -> do
            atomically $ modifyTVar' stateHandlers (M.delete uniq)
            runXMPP state (stanzaHandler stanza)
        Nothing ->
            return ()
    runXMPPLoop' state stanzas

-- | Find handler for the given stanza based on predicates.
findHandler :: XMLElem -> XMPPHandlers -> Maybe (Unique, XMPPHandler)
findHandler stanza = findHandler' stanza . M.toList

findHandler' :: XMLElem -> [(Unique, XMPPHandler)]
             -> Maybe (Unique, XMPPHandler)
findHandler' _ [] = Nothing
findHandler' stanza ((uniq, handler):handlers) =
    if predicate stanza
       then Just (uniq, handler)
       else findHandler' stanza handlers
  where
    predicate = case handler of
        Catch pred' _ -> pred'
        CatchOnce pred' _ -> pred'

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
