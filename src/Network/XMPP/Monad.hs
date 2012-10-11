module Network.XMPP.Monad
    ( XMPP
    , initXMPP
    , runXMPP
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
                               writeTVar, modifyTVar')
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

-- | State which we can read and modify inside a monad.
data XMPPState = forall c. XMPPConnection c => XMPPState
    { stateIsMainLoop :: TVar Bool
    , stateConnection :: c
    , stateHandlers :: TVar XMPPHandlers
    }

-- | XMPP monad.
newtype XMPP a = XMPP { unXMPP :: ReaderT XMPPState IO a }
    deriving (Monad, MonadIO)

-- | Initialize monad state.
initXMPP :: XMPPConnection c => c -> IO XMPPState
initXMPP c = do
    isMain <- newTVarIO False
    handlers <- newTVarIO M.empty
    return $ XMPPState isMain c handlers

-- | Run a function inside the XMPP monad using initialized state.
runXMPP :: XMPPState -> XMPP () -> IO ()
runXMPP state@(XMPPState { .. }) m = do
    isMain <- readTVarIO stateIsMainLoop
    when (not isMain) $
        atomically $ writeTVar stateIsMainLoop True
    runReaderT (unXMPP m) state
    when isMain $
        runXMPPLoop state []

runXMPPLoop :: XMPPState -> [XMLElem] -> IO ()
runXMPPLoop state@(XMPPState { .. }) [] = do
    handlers <- readTVarIO stateHandlers
    when (not $ M.null handlers) $
        getStanzas stateConnection >>= runXMPPLoop state
runXMPPLoop state@(XMPPState { .. }) (stanza:stanzas) = do
    handlers <- readTVarIO stateHandlers
    case findHandler stanza handlers of
        Just (_, Catch _ stanzaHandler) ->
            run (stanzaHandler stanza)
        Just (uniq, CatchOnce _ stanzaHandler) -> do
            atomically $ modifyTVar' stateHandlers (M.delete uniq)
            run (stanzaHandler stanza)
        Nothing ->
            return ()
    runXMPPLoop state stanzas
  where
    run m = runReaderT (unXMPP m) state

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
