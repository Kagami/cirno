{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getProgName, getArgs)

import Network.XMPP (XML(..), openTCPConnection, runXMPPLoop,
                     initStream, sendStanza)

echoBot :: [String] -> IO ()
echoBot [jidS, _password, _jidToS] = do
    let jid = read jidS
    state <- openTCPConnection jid Nothing
    runXMPPLoop state $ do
        initStream
        sendStanza $ XML "test" [] []

--    runXMPP server $ do
--        legacyAuth jid password
--        sendInitialPresence
--        sendMessage jidTo "Oh hai! I'm echo bot. Send me something, plz."
--        addHandler (isMessage & isFrom jidTo) $ \stanza ->
--            sendMessage jidTo (getBody stanza)

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case length args of
        3 -> echoBot args
        _ -> putStrLn $ "Usage: `" ++ progName ++
                        " bot@jabber.org bot_password you@jabber.org'"
