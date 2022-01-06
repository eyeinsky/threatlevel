{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Core
  ( module JS.DSL.Polysemy.Core
  , module Core
--  , module Export
  , Core.State(..)
  , ask, put
  ) where

import Common.Prelude hiding (next)
import X.Prelude ((&), Identity(..))
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HS

import Polysemy hiding (run)
import qualified Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fixpoint

import JS.Syntax as Syntax
import JS.DSL.Core hiding (State)
import qualified JS.DSL.Core as Core
import Identifiers hiding (next)


type List' e =
  [ Writer (Code e)
  , State Idents
  , State Used
  , State Lib
  , Reader Syntax.Conf
  , Fixpoint
  , Final Identity
  ]
type List = List' ()

type BasePoly r = Members List r
type BaseMono a = Sem List a

-- * Effect

data Emit :: Effect where
  GetIdentifier :: Emit m Name
  EmitStatement :: Statement () -> Emit m ()
  -- EmitFunctionDefinition
  --   :: BasePoly r => Name -> Sem (Emit : r) () -> Emit m ()

makeSem ''Emit

-- * Run

runBase
  :: Syntax.Conf -> Lib -> Used -> Idents
  -> BaseMono a -> MonoResult () a
runBase env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity

runAsBase
  :: forall r a
   . BasePoly r
  => Syntax.Conf -> Lib -> Used -> Idents -> Sem (Emit : r) a -> Sem r a
runAsBase env lib used idents = interpret $ \case
  GetIdentifier -> do
    Infinite x xs <- get
    put xs
    return $ Name x
  EmitStatement stm -> tell (pure stm :: Code ())
--   EmitFunctionDefinition name body -> do
-- --    let z = runEmit env lib used idents body :: Sem m ()
--     undefined
--   _ -> undefined

-- runWithBase ::
-- runWithBase runBase' env lib used idents m = runBase' runAsBase m

-- * Polysemy

-- | Fixed effect order similar to MTL
type Mono r = Sem (List' r)
type MonoResult r a = (Lib, (Used, (Idents, (Code r, a))))

type States m =
  ( State Idents `Member` m
  , State Used `Member` m
  , State Lib `Member` m
  )

type C r m =
  ( Writer (Code r) `Member` m
  , States m
  , Reader Syntax.Conf `Member` m
  )

type Poly r m a = C r m => Sem m a

runPolysemy :: Syntax.Conf -> Lib -> Used -> Idents -> Mono r a -> MonoResult r a
runPolysemy env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity

polysemy2common :: MonoResult r a -> Result r a
polysemy2common (lib, (used, (fresh, (code, a)))) =
  ((a, code), Core.State fresh used lib)

-- * API

-- ** Fixed

type M r = Mono r

run :: Syntax.Conf -> Lib -> Used -> Idents -> M r a -> Result r a
run env lib used fresh m = polysemy2common $ runPolysemy env lib used fresh m

-- **  Poly

getState :: States m => Sem m Core.State
getState = do
  fresh :: Idents <- get
  used :: Used <- get
  lib :: Lib <- get
  return $ Core.State fresh used lib

putState :: States m => Core.State -> Sem m ()
putState (Core.State fresh used lib) = put fresh *> put used *>  put lib

getLibrary :: (State Lib `Member` m) => Sem m Lib
getLibrary = get @Lib

modifyLibrary :: (State Lib `Member` m) => (Lib -> Lib) -> Sem m ()
modifyLibrary = modify

askEnv :: (Reader Syntax.Conf `Member` m) => Sem m Syntax.Conf
askEnv = ask @Syntax.Conf

-- | Write JS
write :: forall r m. Writer (Code r) `Member` m => Statement r -> Sem m ()
write stm = tell (pure stm :: Code r)

-- | Get next identifier
next :: State Idents `Member` m => Sem m Name
next = do
  Infinite x xs <- get
  put xs
  return $ Name x

pushName :: State Used `Member` m => TS.Text -> Sem m Name
pushName name = do
  names :: Used <- get
  case HS.lookup name names of
    Just (Infinite s uffixes) -> do
      modify (HS.insert name uffixes)
      return $ Name $ name <> "_" <> s
    _ -> do
      let suffixes = fmap TS.pack $ bigEndian ['a' .. 'z']
      modify @Used (HS.insert name suffixes)
      return $ Name name

mkCode :: forall ra rb m a. Mono rb a -> Poly ra m (Code rb)
mkCode m = do
  fresh0 :: Idents <- get
  used0 :: Used <- get
  lib0 :: Lib <- get
  env :: Syntax.Conf <- ask
  let (lib1, (used1, (fresh', (code, _)))) = runPolysemy @rb env lib0 used0 fresh0 m
  put fresh'
  put used1
  put lib1
  return code

mkCode_ :: forall r m a . Mono r a -> Poly r m (Code r)
mkCode_ = mkCode @r @r
