module CSS.DSL.MTL.Effect where

import Common.Prelude
import Data.Text qualified as TS
import CSS.Syntax

-- * Effect

class Monad m => CSS m where
  css :: m a -> m Class
  rule :: SelectorFrom s => s -> m a -> m a
  atRule :: TS.Text -> TS.Text -> m a -> m a
  combine :: (Selector -> Selector) -> m a -> m a
  execSub :: m a -> m (W, a)

class Monad m => Prop m where
  prop :: TS.Text -> Value -> m ()

-- Writer boilerplate due to writing two things
data W = W { wRules :: OuterRules, wDecls :: Declarations }
  deriving Show
instance Semigroup W where W a b <> W a' b' = W (a <> a') (b <> b')
instance Monoid W where mempty = W mempty mempty

tellRules :: MonadWriter W m => OuterRules -> m ()
tellRules rs = tell (W rs mempty)

tellDecls :: MonadWriter W m => Declarations -> m ()
tellDecls ds = tell (W mempty ds)
--

-- * Compat

combinator :: CSS m => SimpleSelectorFrom a => SOp -> a -> m () -> m ()
combinator op slike = combine (\s -> Combined op s (ssFrom slike))

type CSSF = forall m . CSS m => m () -> m ()
type CSSM = forall m . CSS m => m ()
type PolyProp = forall m . Prop m => m ()
