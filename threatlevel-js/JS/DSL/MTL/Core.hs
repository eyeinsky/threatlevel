{-# OPTIONS_GHC -Wno-orphans #-}
module JS.DSL.MTL.Core
  ( module JS.DSL.Core
  , module JS.DSL.MTL.Core
  , ask, put
  ) where

import Common.Prelude hiding (next)
import qualified Common.Prelude as P
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TL
import qualified Data.HashMap.Strict as HS
import Control.Monad.Writer
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Data.Default
import Control.Lens

import Render
import qualified JS.Syntax as Syntax
import JS.DSL.Core

-- * Monad

type M r = WriterT (Syntax.Code r) (StateT State (Reader Env))

run :: Env -> Lib -> Used -> Fresh -> M r a -> ((a, Syntax.Code r), State)
run env lib used fresh m = m
  & runWriterT
  & flip runStateT (State fresh used lib)
  & flip runReaderT env
  & runIdentity

getState :: M r State
getState = get

putState :: State -> M r ()
putState = put

getLibrary :: M r Lib
getLibrary = gets (^.library)

modifyLibrary :: (Lib -> Lib) -> M r ()
modifyLibrary f = modify (library %~ f)

askEnv :: M r Syntax.Conf
askEnv = ask

write :: Syntax.Statement r -> M r ()
write stm = tell [stm]

-- * Core API

next :: M r Syntax.Name
next = do
  name :: TS.Text <- P.next freshIdentifiers
  names <- gets (view inUseIdentifiers)
  if HS.member name names
    then next
    else return $ Syntax.Name name

pushName :: TS.Text -> M r Syntax.Name
pushName name = do
  names <- gets (view inUseIdentifiers)
  case HS.lookup name names of
    Just (Infinite s uffixes) -> do
      modify (inUseIdentifiers %~ HS.insert name uffixes)
      return $ Syntax.Name $ name <> "_" <> s
    _ -> do
      let suffixes = fmap TS.pack $ bigEndian ['a' .. 'z']
      modify (inUseIdentifiers %~ HS.insert name suffixes)
      return $ Syntax.Name name

{- | Evaluate JSM code to @Code r@

     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: forall ra rb a . M rb a -> M ra (Syntax.Code rb)
mkCode mcode = do
  conf <- ask
  let
    fromNext :: State -> M r t -> (Syntax.Code r, State)
    fromNext (State fresh used lib) m = (w, s1)
      where
        ((_, w), s1) = run conf lib used fresh m
  (w, s1) <- fromNext <$> get <*> pure mcode
  put s1 *> pure w

mkCode_ :: forall r a . M r a -> M r (Syntax.Code r)
mkCode_ = mkCode
