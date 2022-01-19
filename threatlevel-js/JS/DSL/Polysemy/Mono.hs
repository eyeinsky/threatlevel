{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Mono
  ( module JS.DSL.Polysemy.Effect
  , module JS.DSL.Polysemy.Mono
  , module Core
--  , module Export
  , Core.State(..)
  , ask, put
  , module Polysemy
  ) where

import Common.Prelude hiding (next)
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HS

import Polysemy hiding (run)
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fixpoint

import qualified Control.Monad.Reader
import Render

import JS.Syntax as Syntax
import JS.DSL.Core hiding (State)
import qualified JS.DSL.Core as Core

import JS.DSL.Polysemy.Effect

-- * Base

type Base =
  [ Writer Code_
  , State Idents
  , State Used
  , State Lib
  , Reader Syntax.Conf
  , Fixpoint
  , Final Identity
  ]

-- * Polymorphic @Base@

getFreshIdentifierBase :: Members Base r => Sem r Name
getFreshIdentifierBase = do
  Infinite x xs <- get
  put xs
  return $ Name x

getPrefixedIdentifierBase :: Members Base r => TS.Text -> Sem r Name
getPrefixedIdentifierBase name = do
  names :: Used <- get
  case HS.lookup name names of
    Just (Infinite s uffixes) -> do
      modify (HS.insert name uffixes)
      return $ Name $ name <> "_" <> s
    _ -> do
      let suffixes = fmap TS.pack $ bigEndian ['a' .. 'z']
      modify @Used (HS.insert name suffixes)
      return $ Name name

emitStatementBase :: Members Base r => Statement () -> Sem r ()
emitStatementBase stm = tell @Code_ (pure stm)

generateCodeBase
  :: forall r b a . (Members Base b)
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Sem r a -> Sem b Code_
generateCodeBase f body = do
  env :: Syntax.Conf <- ask
  fresh0 :: Idents <- get
  used0 :: Used <- get
  lib0 :: Lib <- get
  let
    f' = f env lib0 used0 fresh0
    (lib1, (used1, (fresh1, (code, _)))) = f' body
  put @Idents fresh1
  put @Used used1
  put @Lib lib1
  return code

-- | Interpret @JS@ as something that has @Base@
jsToBase
  :: forall r b a
   . (Members Base b, r ~ (JS b : b))
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Sem (JS b : b) a -> Sem b a
jsToBase f = interpretH $ \case
  GetFreshIdentifier -> pureT =<< getFreshIdentifierBase
  GetPrefixedIdentifier name -> pureT =<< getPrefixedIdentifierBase name
  EmitStatement stm -> pureT =<< emitStatementBase stm
  GenerateCode body -> pureT =<< generateCodeBase f body

-- * Monomorphic base

type BaseResult a = (Lib, (Used, (Idents, (Code_, a))))

type MonoJS = Sem (JS Base : Base)

-- | Run monomorphic @Base@
runBase  :: RunBase (Sem Base a -> BaseResult a)
runBase env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity

-- | Anything that can run monomorphic base
type RunBase a = Syntax.Conf -> Lib -> Used -> Idents -> a

baseResultCode :: BaseResult a -> Code_
baseResultCode (_, (_, (_, (code, _)))) = code

-- | Run @JS@: just JS is run in terms of @Base e@)
run
  :: forall a
   . ()
  => RunBase (Sem (JS Base : Base) a -> BaseResult a)
run env lib used fresh m = m
  & jsToBase run
  & runBase env lib used fresh

-- * Convenience

runEmpty :: Syntax.Conf -> Sem (JS Base : Base) a -> BaseResult a
runEmpty env m = run env mempty mempty validIdentifiers m

instance Render (MonoJS a) where
  type Conf (MonoJS a) = Syntax.Conf
  renderM m = do
    env <- Control.Monad.Reader.ask
    renderM $ baseResultCode $ runEmpty env m

-- hot = putStrLn "everything compiles"
