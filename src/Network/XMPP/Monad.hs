module Network.XMPP.Monad
    ( XMPP
    , runXMPP
    , XMPPHandler(..)
    , StanzaPredicate
    , StanzaHandler
    ) where

import Control.Monad (when, forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO,
                               writeTVar)

import Network.XMPP.XML (XMLElem)
import Network.XMPP.XMPPConnection (XMPPConnection, getStanzas)

-- | Stanza handler (callback).
data XMPPHandler = Catch StanzaPredicate StanzaHandler
                 | CatchOnce StanzaPredicate StanzaHandler

type StanzaPredicate = XMLElem -> Bool
type StanzaHandler = XMLElem -> XMPP ()

-- | State which we can read and modify inside a monad.
data XMPPState = forall c. XMPPConnection c => XMPPState
    { stateIsMainLoop :: TVar Bool
    , stateConnection :: c
    , stateHandlers :: TVar [XMPPHandler]
    }

-- | XMPP monad.
newtype XMPP a = XMPP { unXMPP :: ReaderT XMPPState IO a }
    deriving (Monad, MonadIO)

-- | Initialize monad state.
initXMPP :: XMPPConnection c => c -> IO XMPPState
initXMPP c = do
    isMain <- newTVarIO False
    handlers <- newTVarIO []
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
    when (not $ null handlers) $
        liftIO (getStanzas stateConnection) >>= runXMPPLoop state
runXMPPLoop state@(XMPPState { .. }) (stanza:stanzas) = do
    handlers <- readTVarIO stateHandlers
    case findHandler stanza handlers of
        Just stanzaHandler ->
            runReaderT (unXMPP $ stanzaHandler stanza) state
        Nothing ->
            return ()
    runXMPPLoop state stanzas

findHandler :: XMLElem -> [XMPPHandler] -> Maybe StanzaHandler
findHandler stanza handlers = Nothing
