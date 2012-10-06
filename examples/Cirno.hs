import Data.Text (Text)
import System.Environment (getProgName, getArgs)

import Network.XMPP ()

echoBot :: String -> String -> String -> a
echoBot = undefined

--echoBot :: JID -> Text -> JID -> XMPP ()
--echoBot jid password jidTo = do
--    let server = jidServer jid
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
    case args of
        [jid, password, jidTo] ->
            echoBot (read jid) (read password) (read jidTo)
        _ ->
            putStrLn $ "Usage: `" ++ progName ++
                       " bot@jabber.org bot_password you@jabber.org'"
