module Base where

import qualified Data.Text as T

type Text   = T.Text
class E a where
   ev :: a -> T.Text
