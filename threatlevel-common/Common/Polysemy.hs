module Common.Polysemy
  ( module Common.Polysemy
  , module Export
  ) where

import Data.Kind
import Polysemy as Export
import Polysemy.Internal as Export (Append)

type RecEff :: EffectRow -> EffectRow -> (EffectRow -> Effect) -> Constraint
type RecEff s r f = (Member (f s) r, r ~ (f s : s))
