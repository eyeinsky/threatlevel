{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Core
  ( module JS.DSL.Polysemy.Core
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

import JS.Syntax as Syntax
import JS.DSL.Core hiding (State)
import qualified JS.DSL.Core as Core

type Base =
  [ Writer Code_
  , State Idents
  , State Used
  , State Lib
  , Reader Syntax.Conf
  , Fixpoint
  , Final Identity
  ]

type BaseMono {-a-} = Sem Base {-a-}
type BaseResult a = (Lib, (Used, (Idents, (Code_, a))))

baseResultCode :: BaseResult a -> Code_
baseResultCode (lib, (used, (fresh, (code, a)))) = code

-- * Effect

-- @r@: row, only used for DefineFunction to have fbody row the same as the outer
-- @m r@: the Effect
data JS b m a where
  GetFreshIdentifier :: JS b m Name
  GetPrefixedIdentifier :: TS.Text -> JS b m Name
  EmitStatement :: Statement -> JS b m ()
  DefineFunction
    :: ()
    => Name
    -> Sem (JS b : b) _a -> JS b m Code_
  -- ^ @s@ is the monomorphic rest of the effects under JS that need
  -- to contain Base. It's required because otherwise from final type
  -- @JS e m ()@ (the @s@ omitted) we wouldn't know what the sub-row
  -- was.

-- !! JS CARRIES BASE !!

makeSem ''JS

-- * Run

-- | Interpret JS in terms of something that hase base
jsToBase
  :: forall b a . (Members Base b)
  => (forall _a . RunBase (Sem (JS b : b) _a -> BaseResult _a))
  -- ^ run sub-JS to syntax syntax
  -> Sem (JS b : b) a -> Sem b a
jsToBase f = interpret $ \case
  GetFreshIdentifier -> do -- next
    Infinite x xs <- get
    put xs
    return $ Name x
  GetPrefixedIdentifier name -> do -- pushName
    names :: Used <- get
    case HS.lookup name names of
      Just (Infinite s uffixes) -> do
        modify (HS.insert name uffixes)
        return $ Name $ name <> "_" <> s
      _ -> do
        let suffixes = fmap TS.pack $ bigEndian ['a' .. 'z']
        modify @Used (HS.insert name suffixes)
        return $ Name name
  EmitStatement stm -> tell @Code_ (pure stm) -- write
  DefineFunction name (body :: Sem (JS b : b) _a) -> do
    env :: Syntax.Conf <- ask
    fresh0 :: Idents <- get
    used0 :: Used <- get
    lib0 :: Lib <- get
    let
      f' :: Sem (JS b : b) _a -> BaseResult _a
      f' = f env lib0 used0 fresh0
      (lib1, (used1, (fresh1, (code, a)))) = f' body
    put @Idents fresh1
    put @Used used1
    put @Lib lib1
    tell @Code_ $ pure $ Syntax.BareExpr $ Syntax.AnonFunc (Just name) [] code
    return code

type MonoJS = Sem (JS Base : Base)

-- | Anything that can run monomorphic base
type RunBase a = Syntax.Conf -> Lib -> Used -> Idents -> a
-- | Run monomorphic JS to result
type RunMono a = RunBase (MonoJS a -> BaseResult a)

-- | Run @JS@: just JS is run in terms of @Base e@)
run :: forall a . RunMono a
run env lib used fresh m = m
  & jsToBase run
  & runBase env lib used fresh

-- | Run @Base e@
runBase  :: RunBase (Sem Base a -> (BaseResult a))
runBase env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity
