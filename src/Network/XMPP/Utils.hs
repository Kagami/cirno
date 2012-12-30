module Network.XMPP.Utils
    ( ReadT(..)
    , showT
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Convert Text to value.
-- Like Prelude.Read but convert from Text instead of String.
class ReadT a where
    readT :: Text -> a

-- | Convert value to Text.
-- Like Prelude.show but convert to Text instead of String.
showT :: Show a => a -> Text
showT = T.pack . show
