module Network.XMPP.JID.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Network.XMPP (JID(..), bareJID, fullJID, readT)

tests :: Test
tests = testGroup "Network.XMPP.JID.Tests"
    [ testCase "Simple" simple
    , testCase "No resource" noResource
    , testCase "Parse from Data.Text" fromText
    , testCase "Parse from Data.Text without resource" fromTextNoResource
    ]
  where
    simple = do
        let jid = read "user@example.com/resource"
        jidUsername jid @=? "user"
        jidServer jid @=? "example.com"
        jidResource jid @=? "resource"
        bareJID jid @=? "user@example.com"
        fullJID jid @=? "user@example.com/resource"
    noResource = do
        let jid = read "user@example.com"
        jidResource jid @=? ""
        bareJID jid @=? fullJID jid
    fromText = do
        let jid = readT "user@example.com/resource"
        fullJID jid @=? "user@example.com/resource"
    fromTextNoResource = do
        let jid = readT "user@example.com"
        bareJID jid @=? "user@example.com"
        fullJID jid @=? "user@example.com"
