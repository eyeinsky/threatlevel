module CSS.DSL.Polysemy
  ( module CSS.DSL.Polysemy
  , module Export
  ) where

import Common.Prelude
import Polysemy
import qualified Data.Text as TS

import CSS.Syntax
import CSS.DSL.Common as Export
import CSS.DSL.Polysemy.Effect as Export


-- | Generate a class, bind declarations @m@ to it
css :: forall s r a . Has s r CSS => Sem r a -> Sem r Class
css m = do
  c <- getFreshClass @s
  emitFor @s (selFrom c) m
  return c

combinator
  :: forall s r a . (Has s r CSS, SimpleSelectorFrom a)
  => SOp -> a -> Sem r () -> Sem r ()
combinator op slike m = do
  selector <- getSelector @s
  emitFor (Combined op selector (ssFrom slike)) m

-- * Derived

type CSSF = forall s r . Has s r CSS => Sem r () -> Sem r ()
type CSSM = forall s r . Has s r CSS => Sem r ()
type PolyProp = forall r . Member Prop r => Sem r ()

combine
  :: forall s r . Has s r CSS
  => (Selector -> Selector) -> Sem r () -> Sem r ()
combine mod m = getSelector @s >>= mod ^ flip emitFor m

atRule
  :: forall s r . Has s r CSS
  => TS.Text -> TS.Text -> Sem r () -> Sem r ()
atRule ruleName rule m = do
  s <- getSelector @s
  rules <- execDsl_ @s m
  emitRules @s $ pure $ AtRule ruleName rule rules

-- * Compat

cssRule
  :: forall s r a
   . (Has s r CSS, SelectorFrom a)
  => a -> Sem r () -> Sem r ()
cssRule slike = emitFor (selFrom slike)
