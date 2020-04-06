module JS.Event where

import qualified Data.Text as TS

import X.Prelude
-- import JS.BuiltIn

class Show a => Event a where
  eventString :: a -> TS.Text
  eventString a = TS.toLower $ TS.pack $ show a

-- | Make 'on$event' attribute
toOn :: Event a => a -> TS.Text
toOn = ("on"<>) . eventString
