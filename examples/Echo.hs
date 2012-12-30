{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromJust)
import Network (withSocketsDo)
import System.Environment (getProgName, getArgs)
import qualified Data.Text as T

import Network.XMPP (XML(..), openTCPConnection, runXMPPLoop, initStream,
                     legacyAuth, sendInitialPresence, sendMessage, addHandler,
                     (&), isChat, isFromBare, getBody)

echoBot :: [String] -> IO ()
echoBot [jidS, passwordS, jidToS] = withSocketsDo $ do
    let jid = read jidS
    let password = T.pack passwordS
    let jidTo = T.pack jidToS
    state <- openTCPConnection jid Nothing
    runXMPPLoop state $ do
        initStream
        legacyAuth password
        sendInitialPresence
        sendMessage jidTo "Oh hai! I'm echo bot. Send me something, plz."
        addHandler (isChat & isFromBare jidTo) $ \stanza ->
            sendMessage jidTo (fromJust $ getBody stanza)

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case length args of
        3 -> echoBot args
        _ -> putStrLn $ "Usage: `" ++ progName ++
                        " bot@jabber.org bot_password you@jabber.org'"
