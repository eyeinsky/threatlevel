module Common where

import Prelude2
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader

type Text = T.Text

type ER = Reader Int T.Text
class E a where
   ev :: a -> ER -- T.Text

-- * Helpers

runPrint = flip runReader 0
mseq li = mconcat <$> sequence li

