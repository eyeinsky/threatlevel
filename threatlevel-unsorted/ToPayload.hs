module ToPayload where

import Common.Prelude
import Data.Text.Lazy qualified as TL

class ToPayload a where
   toPayload :: a -> TL.Text

instance ToPayload TL.Text where
   toPayload = id

pair j (a,b) = a <> j <> b
un x xs = TL.intercalate x xs
