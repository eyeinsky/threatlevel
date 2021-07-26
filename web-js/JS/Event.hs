module JS.Event where

import Prelude
import qualified Data.Text as TS

class Show a => Event a where
  eventString :: a -> TS.Text
  eventString a = TS.toLower $ TS.pack $ show a

-- | Make 'on$event' attribute
toOn :: Event a => a -> TS.Text
toOn = ("on"<>) . eventString
