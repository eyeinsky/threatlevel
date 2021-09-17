module Common.Prelude
  ( module Common.Prelude
  , module Export
  ) where

import Prelude as Export
  hiding ((^), (/), const, rem, div, log, span)
import Data.Maybe as Export
import Data.String as Export
import Control.Monad as Export
import Data.Functor as Export
import Data.Foldable as Export
import Control.Lens as Export hiding
  ((.=), (.>), Empty, Setter, Getter, Const, Context, transform)
import Control.Arrow as Export hiding (left, right)

infixr 9 ^
(^) = flip (.)

todo :: a
todo = undefined

-- Data.Bool
infix 1 ?
(?) bool = if bool then (\a _ -> a) else (\_ a -> a)
