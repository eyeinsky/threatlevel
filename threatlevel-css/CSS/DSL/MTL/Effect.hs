module CSS.DSL.MTL.Effect where

import Common.Prelude
import Data.Text qualified as TS
import CSS.Syntax

-- * Effect

class Monad m => CSS m where
  css
    :: m a     -- ^ rules and declarations to generated class
    -> m Class -- ^ returns a fresh class with rules attached
  rule
    :: SelectorFrom s
    => s   -- ^ selector-like to which the rule applies
    -> m a -- ^ rules, declarations and all else available in effect @m@
    -> m a
  atRule
    :: TS.Text -- ^ at-rule name, e.g @media@
    -> TS.Text -- ^ additional conditions e.g @(min-width: 1024px)@
    -> m a     -- ^ rules, declarations and all else available in effect @m@
    -> m a
  combine :: (Selector -> Selector) -> m a -> m a
  execSub :: m a -> m (a, W)

class Monad m => Prop m where
  prop :: TS.Text -> Value -> m ()

-- Writer boilerplate due to writing two things
data W = W { wRules :: OuterRules, wDecls :: Declarations }
  deriving Show
instance Semigroup W where W a b <> W a' b' = W (a <> a') (b <> b')
instance Monoid W where mempty = W mempty mempty
--

-- * Compat

-- | Combine current selector with another non-simple selector
combinator :: CSS m => SimpleSelectorFrom a => SOp -> a -> m () -> m ()
combinator op slike = combine (\s -> Combined op s (ssFrom slike))

type CSSF = forall m . CSS m => m () -> m ()
type CSSM = forall m . CSS m => m ()
type PolyProp = forall m . Prop m => m ()
