{-# LANGUAGE AllowAmbiguousTypes #-}
module CSS.DSL.Polysemy.Effect where

import Common.Prelude
import Common.Polysemy
import qualified Data.Text as TS

import Polysemy
import Polysemy.Internal
import CSS.Syntax
import CSS.DSL.Common

data Prop :: Effect where
  Prop :: TS.Text -> Value -> Prop m ()

makeSem ''Prop

-- * CSS

data CSS :: Effect where
  Css :: m a -> CSS m Class
  GetFreshClass :: CSS m Class
  GetSelector :: CSS m Selector
  EmitFor :: Selector -> m  a -> CSS m a
  EmitRules :: OuterRules -> CSS m ()
  ExecDsl :: m a -> CSS m (OuterRules, a)

makeSem ''CSS

combinator
  :: forall r a . (Member CSS r, SimpleSelectorFrom a)
  => SOp -> a -> Sem r () -> Sem r ()
combinator op slike m = do
  selector <- getSelector
  emitFor (Combined op selector (ssFrom slike)) m

-- | Helper to make at rules.
atRule
  :: forall r . Member CSS r
  => TS.Text -> TS.Text -> Sem r () -> Sem r ()
atRule ruleName rule m = do
  rules <- execDsl m <&> fst
  emitRules $ pure $ AtRule ruleName rule rules

cssRule :: (Member CSS r, SelectorFrom a) => a -> Sem r () -> Sem r ()
cssRule slike = emitFor (selFrom slike)

-- * Compat

-- | Helper for CSS.TH. Run @m@ for selector modified by @mod@.
combine
  :: forall s r . RecEff s r CSS
  => (Selector -> Selector) -> Sem r () -> Sem r ()
combine mod m = getSelector @s >>= mod ^ flip emitFor m

type CSSF = forall r . Member CSS r => Sem r () -> Sem r ()
type CSSM = forall r . Member CSS r => Sem r ()
type PolyProp = forall r . Member Prop r => Sem r ()
