module Common
   ( module Common
   , runReader, ask
   ) where

import Prelude2
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader

type Text = T.Text

type ER = Reader Int
class E a where
   ev :: a -> ER T.Text

increaseIndent = withReader (+2)

runPrint :: ER T.Text -> T.Text
runPrint = flip runReader 0

-- * Helpers

mseq li = mconcat <$> sequence li
