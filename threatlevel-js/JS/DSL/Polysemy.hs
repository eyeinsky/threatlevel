{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy
  ( module JS.DSL.Polysemy
  , module Core
  , Core.State(..)
  , ask, put
  ) where

import Prelude
import X.Prelude ((&), Identity(..))
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HS

import Polysemy hiding (run)
import qualified Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fixpoint
-- import Polysemy.Identity

import JS.Syntax as Syntax
import JS.DSL.Core hiding (State)
import qualified JS.DSL.Core as Core
import Identifiers

-- * Polysemy

-- | Fixed effect order similar to MTL
type Fixed r = Sem
  '[ Writer (Code r)
   , State Idents
   , State Used
   , State Lib
   , Reader Syntax.Conf
   , Fixpoint
   , Final Identity
   ]
type FixedResult r a = (Lib, (Used, (Idents, (Code r, a))))

type States m =
  ( State Idents `Member` m
  , State Used `Member` m
  , State Lib `Member` m
  )

type Poly r m a =
  ( Writer (Code r) `Member` m
  , States m
  , Reader Syntax.Conf `Member` m
  ) => Sem m a

runPolysemy :: Syntax.Conf -> Lib -> Used -> Idents -> Fixed r a -> FixedResult r a
runPolysemy env lib used fresh m = m
  & runWriter
  & runState fresh
  & runState used
  & runState lib
  & runReader env
  & fixpointToFinal @Identity
  & runFinal
  & runIdentity
--  & Polysemy.run

polysemy2common :: FixedResult r a -> Result r a
polysemy2common (lib, (used, (fresh, (code, a)))) =
  ((a, code), Core.State fresh used lib)

-- * API

-- ** Fixed

type M r = Fixed r

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

mkCode :: forall ra rb m a. Fixed rb a -> Poly ra m (Code rb)
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

mkCode_ :: forall r m a . Fixed r a -> Poly r m (Code r)
mkCode_ = mkCode @r @r
