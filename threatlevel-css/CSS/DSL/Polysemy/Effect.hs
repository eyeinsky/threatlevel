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

data CSS (s :: EffectRow) :: Effect where
  GetFreshClass :: CSS s m Class
  GetSelector :: CSS s m Selector
  EmitFor :: Selector -> Sem (CSS s : s) a -> CSS s m a
  EmitRules :: OuterRules -> CSS s m ()

  ExecDsl :: Sem (CSS s : s) a -> CSS s m (OuterRules, a)

makeSem ''CSS

-- | Generate a class, bind declarations @m@ to it
css :: forall s r a . RecEff s r CSS => Sem r a -> Sem r Class
css m = do
  c <- getFreshClass @s
  emitFor @s (selFrom c) m
  return c

combinator
  :: forall s r a . (RecEff s r CSS, SimpleSelectorFrom a)
  => SOp -> a -> Sem r () -> Sem r ()
combinator op slike m = do
  selector <- getSelector @s
  emitFor (Combined op selector (ssFrom slike)) m

-- | Helper to make at rules.
atRule
  :: forall s r . RecEff s r CSS
  => TS.Text -> TS.Text -> Sem r () -> Sem r ()
atRule ruleName rule m = do
  s <- getSelector @s
  rules <- execDsl m <&> fst
  emitRules @s $ pure $ AtRule ruleName rule rules

-- | Helper for CSS.TH. Run @m@ for selector modified by @mod@.
combine
  :: forall s r . RecEff s r CSS
  => (Selector -> Selector) -> Sem r () -> Sem r ()
combine mod m = getSelector @s >>= mod ^ flip emitFor m

cssRule
  :: forall s r a
   . (RecEff s r CSS, SelectorFrom a)
  => a -> Sem r () -> Sem r ()
cssRule slike = emitFor (selFrom slike)

-- * Compat

type CSSF = forall s r . RecEff s r CSS => Sem r () -> Sem r ()
type CSSM = forall s r . RecEff s r CSS => Sem r ()
type PolyProp = forall r . Member Prop r => Sem r ()
