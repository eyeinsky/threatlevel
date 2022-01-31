module Common.Polysemy where

import Data.Kind
import Polysemy

type RecEff :: EffectRow -> EffectRow -> (EffectRow -> Effect) -> Constraint
type RecEff s r f = (Member (f s) r, r ~ (f s : s))
