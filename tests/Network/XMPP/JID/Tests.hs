module Network.XMPP.JID.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Network.XMPP (JID(..), bareJid, fullJid, readT)

tests :: Test
tests = testGroup "Network.XMPP.JID.Tests"
    [ testCase "Simple" simple
    , testCase "No resource" noResource
    , testCase "Parse from Data.Text" parseFromText
    ]
  where
    simple = do
        let jid = read "user@example.com/resource"
        jidUsername jid @=? "user"
        jidServer jid @=? "example.com"
        jidResource jid @=? "resource"
        bareJid jid @=? "user@example.com"
        fullJid jid @=? "user@example.com/resource"
    noResource = do
        let jid = read "user@example.com"
        jidResource jid @=? ""
        bareJid jid @=? fullJid jid
    parseFromText = do
        let jid = readT "user@example.com/resource"
        fullJid jid @=? "user@example.com/resource"
