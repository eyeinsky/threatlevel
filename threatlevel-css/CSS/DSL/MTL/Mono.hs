{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CSS.DSL.MTL.Mono
  ( module CSS.DSL.MTL.Effect
  , module CSS.DSL.MTL.Mono
  , Conf(..)
  ) where

import Common.Prelude
import Data.Text qualified as TS
import CSS.Syntax
import CSS.DSL.Common

import Data.Tuple (swap)
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any)
import Control.Monad.State

import Render qualified

import CSS.DSL.MTL.Effect

-- * Base

cssBase get put m = do
  Infinite x xs <- get
  put xs
  let class_ = Class x
  rule class_ m
  return class_

ruleBase local tell slike m =
  let s = selFrom slike
  in local (\_ -> selFrom s) $ do
    (a, W rs ds) <- execSub m
    let rs = pure (mkRule s ds) <> rs
    tell (W rs mempty)
    return a

combineBase ask f m = do
  selector <- ask
  rule (f selector) m

atRuleBase ask tell atIdent atCond m = do
  (a, w) <- execSub m
  selector <- ask
  let rs = pure $ AtRule atIdent atCond $ wrapW selector w
  tell (W rs mempty)
  return a

execSubBase ask get put run m = do
  selector <- ask
  fresh0 <- get
  let ((a, rs), fresh1) = run selector fresh0 m
  put fresh1
  return (a, rs)

propBase tell property value = do
  let ds = pure $ Declaration property value
  tell (W mempty ds)

-- * Mono CSS

type State_ = Names
type Reader_ = Selector
type Writer_ = W

type MonoCSS' m = WriterT Writer_ (StateT State_ (ReaderT Reader_ m))
type MonoCSS = WriterT Writer_ (StateT State_ (Reader Reader_))

runM :: Reader_ -> State_ -> MonoCSS' m a -> m ((a, Writer_), State_)
runM r s m = m
  & runWriterT
  & flip runStateT s
  & flip runReaderT r

run :: Reader_ -> State_ -> MonoCSS a -> ((a, W), Names)
run r s m = m
  & runM r s
  & runIdentity

-- | Exec @MonoCSS@ to @OuterRules@
runWrapRules :: Reader_ -> State_ -> MonoCSS a -> BaseResult a
runWrapRules r s m = m
  & run r s
  & \((a, w), names) -> ((a, wrapW r w), names)

-- | Wrap declarations @ds@ into a rule with selector @r@
wrapW :: Selector -> W -> OuterRules
wrapW r (W rs ds) = let bare = mkRule r ds in pure bare <> rs

runFresh :: MonoCSS a -> BaseResult a
runFresh m = runWrapRules (selFrom $ PseudoClass "host" Nothing) identifiers m

instance CSS MonoCSS where
  css m = cssBase get put m
  rule slike m = ruleBase local tell slike m
  combine f m = combineBase ask f m
  atRule atIdent atCond m = atRuleBase ask tell atIdent atCond m
  execSub m = execSubBase ask get put run m

instance Prop MonoCSS where
  prop property value = propBase tell property value

instance Render.Render (MonoCSS a) where
  type Conf (MonoCSS a) = Conf
  renderM = runFresh ^ getWriter ^ Render.renderM

-- * Mono Prop

type MonoProp' m = WriterT Declarations m
type MonoProp = MonoProp' Identity

instance Prop MonoProp where
  prop property value = tell $ pure $ Declaration property value

runMonoProp :: MonoProp a -> (a, Declarations)
runMonoProp = runWriter

instance Render.Render (MonoProp a) where
  type Conf (MonoProp a) = Conf
  renderM m = Render.renderM w
    where (_, w) = runMonoProp m
