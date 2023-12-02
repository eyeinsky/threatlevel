{-# OPTIONS_GHC -Wno-orphans #-}

module JS.DSL.MTL.Mono
  ( module JS.DSL.MTL.Mono
  , module JS.DSL.MTL.Effect
  , module JS.DSL.Core
  ) where

import Common.Prelude
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any)
import Control.Monad.State hiding (State)

import Render

import JS.Syntax as Syntax
import JS.DSL.Core
import JS.DSL.MTL.Effect

-- * MTL base

stmBase :: Applicative f => (f a -> c) -> a -> c
stmBase tell = tell . pure

freshNameBase :: Monad m => m State -> (State -> m a) -> m Name
freshNameBase get put = do
  State (Infinite x xs) used lib <- get
  put (State xs used lib) $> Name x

bindBase :: JS m => (Name -> t -> Statement ()) -> t -> m (Expr a)
bindBase syntax e = do
  name <- freshName
  stm (syntax name e) $> EName name

execSubBase :: Monad m =>
               m t1
               -> m t2
               -> (t3 -> m a1)
               -> (t1 -> t2 -> t4 -> ((a2, b), t3))
               -> t4
               -> m (a2, b)
execSubBase ask get put run m = do
  env <- ask
  state0 <- get
  let ((a, w), s) = run env state0 m
  put s $> (a, w)

-- * Mono

type State_ = JS.DSL.Core.State
type Reader_ = Env
type Writer_ = Code_

type MonoJS' m = WriterT Writer_ (StateT State_ (ReaderT Reader_ m))
type MonoJS = MonoJS' Identity
type Result' a = (State_, (Writer_, a))

runM :: Reader_ -> State_ -> MonoJS' m a -> m ((a, Writer_), State_)
runM r s m = m
  & runWriterT
  & flip runStateT s
  & flip runReaderT r

run :: Reader_ -> State_ -> MonoJS a -> Result () a
run r s m = m
  & runM r s
  & runIdentity

runFresh :: Syntax.Conf -> MonoJS a -> Result () a
runFresh env m = run env (State validIdentifiers mempty mempty) m

instance JS MonoJS where
  stm = stmBase tell
  freshName = freshNameBase get put
  bind = bindBase
  execSub = execSubBase ask get put run
  execFunc f = c2 f []

  -- func syntax f = bind Let . uncurry (syntax Nothing) =<< funcUntyped f

-- * Render

instance Render (MonoJS a) where
  type Conf (MonoJS a) = Syntax.Conf
  renderM m = renderM . resultCode . flip runFresh m =<< ask

runPretty :: MonoJS a -> Result () a
runPretty = runFresh (Indent 2)

runMinify :: MonoJS a -> Result () a
runMinify = runFresh Minify
