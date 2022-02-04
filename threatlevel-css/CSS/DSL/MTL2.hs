module CSS.DSL.MTL2 where

import Common.Prelude
import Data.Text qualified as TS
import CSS.Syntax
import CSS.DSL.Common

import Data.Tuple (swap)
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any)
import Control.Monad.State

import Render qualified

-- * Effect

class Monad m => CSS m where
  css :: m a -> m Class
  rule :: SelectorFrom s => s -> m a -> m a
  atRule :: TS.Text -> TS.Text -> m a -> m a
  combine :: (Selector -> Selector) -> m a -> m a
  execSub :: m a -> m (W, a)

class Monad m => Prop m where
  prop :: TS.Text -> Value -> m ()

-- * Base

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

type MonoCSS = WriterT W (StateT Names (Reader Selector))
type Result a = (Names, (W, a))

run :: Selector -> Names -> MonoCSS a -> Result a
run r s m = m
  & runWriterT
  & (flip runStateT s ^ fmap swap)
  & (flip runReader r ^ fmap swap)

-- | Exec @MonoCSS@ to @OuterRules@
runWrapRules :: Selector -> Names -> MonoCSS a -> BaseResult a
runWrapRules r s m = m
  & run r s
  & \(names, (w, a)) -> (names, (wrapW r w, a))

-- | Wrap declarations @ds@ into a rule with selector @r@
wrapW :: Selector -> W -> OuterRules
wrapW r (W rs ds) = let bare = mkRule r ds in pure bare <> rs

runFresh :: MonoCSS a -> BaseResult a
runFresh m = runWrapRules (selFrom Any) identifiers m

instance CSS MonoCSS where
  css m = do
    Infinite x xs <- get
    put xs
    let class_ = Class x
    rule class_ m
    return class_
  rule slike m = let s = selFrom slike
    in local (\_ -> selFrom s) $ do
      (W rs ds, a) <- execSub m
      tellRules (pure (mkRule s ds) <> rs)
      return a
  combine f m = do
    selector <- ask
    rule (f selector) m
  atRule atIdent atCond m = do
    (w, a) <- execSub m
    selector <- ask
    tellRules (pure $ AtRule atIdent atCond $ wrapW selector w)
    return a

  execSub m = do
    selector <- ask
    fresh0 <- get
    let (fresh1, (rs, a)) = run selector fresh0 m
    put fresh1
    return (rs, a)

instance Prop MonoCSS where
  prop property value = do
    tellDecls $ pure $ Declaration property value

instance Render.Render (MonoCSS a) where
  type Conf (MonoCSS a) = Conf
  renderM = runFresh ^ getWriter ^ Render.renderM

-- * Compat

combinator :: CSS m => SimpleSelectorFrom a => SOp -> a -> m () -> m ()
combinator op slike = combine (\s -> Combined op s (ssFrom slike))

type CSSF = forall m . CSS m => m () -> m ()
type CSSM = forall m . CSS m => m ()
type PolyProp = forall m . Prop m => m ()
