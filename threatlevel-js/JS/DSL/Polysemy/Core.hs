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

type Base e =
  [ Writer (Code e)
  , State Idents
  , State Used
  , State Lib
  , Reader Syntax.Conf
  , Fixpoint
  , Final Identity
  ]

type BaseMono e {-a-} = Sem (Base e) {-a-}
type BaseResult r a = (Lib, (Used, (Idents, (Code r, a))))


-- * Effect

-- @r@: row, only used for DefineFunction to have fbody row the same as the outer
-- @e@: return type of the current monad expression
-- @m r@: the Effect
data JS r e m a where
  GetFreshIdentifier :: JS r e m Name
  GetPrefixedIdentifier :: TS.Text -> JS r e m Name
  EmitStatement :: Statement e -> JS r e m ()
  DefineFunction
    :: (Base _e ~ r)
    => Name
    -> Sem (JS r _e : r) _a -> JS r e m (Code _e)
  -- ^ @s@ is the monomorphic rest of the effects under JS that need
  -- to contain Base. It's required because otherwise from final type
  -- @JS e m ()@ (the @s@ omitted) we wouldn't know what the sub-row
  -- was.

makeSem ''JS

-- * Run

-- | Interpret JS in terms of Base
jsToBase
  :: forall r e a . Members (Base e) r
  => (forall _e _a . RunMono _e _a)
  -- ^ run sub-JS to syntax syntax
  -> Sem (JS r e : r) a -> Sem r a
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
  EmitStatement stm -> tell (pure stm :: Code e) -- write
  DefineFunction name (body :: Sem (JS r _e : r) _a) -> do
    env :: Syntax.Conf <- ask
    fresh0 :: Idents <- get
    used0 :: Used <- get
    lib0 :: Lib <- get
    let
      f' :: Sem (JS r _e : r) _a -> BaseResult _e _a
      f' = f env lib0 used0 fresh0
      (lib1, (used1, (fresh1, (code, a)))) = f' body
    put @Idents fresh1
    put @Used used1
    put @Lib lib1
    tell @(Code e) $ pure $ Syntax.BareExpr $ Syntax.AnonFunc (Just name) [] code
    return code

-- | Anything that can run monomorphic base
type RunBase a = Syntax.Conf -> Lib -> Used -> Idents -> a
-- | Run monomorphic JS to result
type RunMono e a = RunBase (Sem (JS (Base e) e : (Base e)) a -> BaseResult e a)

-- | Run @JS@: just JS is run in terms of @Base e@)
run :: forall e a . RunMono e a
run env lib used fresh m = m
  & jsToBase run
  & runBase env lib used fresh

-- | Run @Base e@
runBase  :: RunBase (Sem (Base e) a -> (BaseResult e a))
runBase env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity

getCode (lib, (used, (fresh, (code, a)))) = code

-- * Old API defined in terms of JS

runBaseMonoToResult :: Syntax.Conf -> Lib -> Used -> Idents -> Sem (Base r) a -> Result r a
runBaseMonoToResult env lib used fresh m = polysemy2common $ runBase env lib used fresh m
  where
  polysemy2common :: BaseResult r a -> Result r a
  polysemy2common (lib, (used, (fresh, (code, a)))) =
    ((a, code), Core.State fresh used lib)
