import Test.Framework (defaultMain)

import qualified Network.XMPP.JID.Tests

main :: IO ()
main = defaultMain
    [ Network.XMPP.JID.Tests.tests
    ]
