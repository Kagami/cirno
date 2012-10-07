import Data.Text (Text)
import Control.Applicative ((<$>))
import System.Environment (getProgName, getArgs)

echoBot :: Text -> Text -> Text -> a
echoBot = undefined

--echoBot :: Text -> Text -> Text -> XMPP ()
--echoBot jid password jidTo = do
--    let server = jidServer $ readT jid
--    runXMPP server $ do
--        legacyAuth jid password
--        sendInitialPresence
--        sendMessage jidTo "Oh hai! I'm echo bot. Send me something, plz."
--        addHandler (isMessage & isFrom jidTo) $ \stanza ->
--            sendMessage jidTo (getBody stanza)

main :: IO ()
main = do
    progName <- getProgName
    args <- map read <$> getArgs
    case args of
        [jid, password, jidTo] ->
            echoBot jid password jidTo
        _ ->
            putStrLn $ "Usage: `" ++ progName ++
                       " bot@jabber.org bot_password you@jabber.org'"
