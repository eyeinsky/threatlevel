{-# LANGUAGE AllowAmbiguousTypes #-}
module CSS.DSL.Polysemy.Effect where

import Common.Prelude
import qualified Data.Text as TS

import Polysemy
import Polysemy.Internal
import CSS.Syntax
import CSS.DSL.Common


data CSS (s :: EffectRow) :: Effect where
  GetFreshClass :: CSS s m Class
  GetSelector :: CSS s m Selector
  EmitFor :: Selector -> Sem (CSS s : s) a -> CSS s m a
  EmitRules :: Rules -> CSS s m ()

  ExecDsl :: Sem (CSS s : s) a -> CSS s m (Rules, a)

makeSem ''CSS


type Has :: EffectRow -> EffectRow -> (EffectRow -> Effect) -> Constraint
type Has s r f = (Member (f s) r, r ~ (f s : s))

-- * Helpers

execDsl_
  :: forall s r a . Has s r CSS
  => Sem (CSS s : s) a -> Sem (CSS s : s) Rules
execDsl_ m = execDsl m <&> fst
