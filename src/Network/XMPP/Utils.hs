module Network.XMPP.Utils
    ( ReadT(..)
    , ShowT(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Convert 'Text' to value.
-- Like 'Read' but convert from 'Text' instead of 'String'.
class ReadT a where
    readT :: Text -> a

-- | Convert value to 'Text'.
-- Like 'Show' but convert to 'Text' instead of 'String'.
class ShowT a where
    showT :: a -> Text

instance ShowT Int where
    showT = T.pack . show
