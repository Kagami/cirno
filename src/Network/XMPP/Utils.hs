module Network.XMPP.Utils
    ( ReadT(..)
    ) where

import Data.Text (Text)

-- | Convert Text to value.
-- Like Prelude.Read but convert from Text instead of String.
-- Don't know why Data.Text module doesn't provide such helper.
class ReadT a where
    readT :: Text -> a