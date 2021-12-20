{-# OPTIONS_GHC -Wno-orphans #-}
module JS.DSL.MTL.Core
  ( module JS.DSL.Core
  , module JS.DSL.MTL.Core
  ) where

import Prelude
import Data.Void
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TL
import qualified Data.HashMap.Strict as HS
import Control.Monad.Writer
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Data.Default
import Control.Lens

import Render
import qualified Identifiers as I
import qualified JS.Syntax as Syntax
import JS.DSL.Core

-- * Monad

type M r = WriterT (Syntax.Code r) (StateT State (Reader Env))

run :: Env -> Fresh -> Used -> Lib -> M r a -> ((a, Syntax.Code r), State)
run env fresh used lib m = m
  & runWriterT
  & flip runStateT (State fresh used lib)
  & flip runReaderT env
  & runIdentity

getState :: M r State
getState = undefined

write :: Syntax.Statement r -> M r ()
write stm = tell [stm]

-- * Core API

next :: M r Syntax.Name
next = do
  name :: TS.Text <- I.next freshIdentifiers
  names <- gets (view inUseIdentifiers)
  if HS.member name names
    then next
    else return $ Syntax.Name name

pushName :: TS.Text -> M r Syntax.Name
pushName name = do
  names <- gets (view inUseIdentifiers)
  case HS.lookup name names of
    Just (I.Infinite s uffixes) -> do
      modify (inUseIdentifiers %~ HS.insert name uffixes)
      return $ Syntax.Name $ name <> "_" <> s
    _ -> do
      let suffixes = fmap TS.pack $ I.bigEndian ['a' .. 'z']
      modify (inUseIdentifiers %~ HS.insert name suffixes)
      return $ Syntax.Name name

{- | Evaluate JSM code to @Code r@

     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (Syntax.Code sub)
mkCode mcode = do
  conf <- ask
  let
    fromNext :: State -> M r t -> (Syntax.Code r, State)
    fromNext (State fresh used lib) m = (w, s1)
      where
        ((_, w), s1) = run conf fresh used lib m
  (w, s1) <- fromNext <$> get <*> pure mcode
  put s1 *> pure w

-- | Runs code in another context with no ability to return
noReturn :: M Void a -> M parent ()
noReturn mcode = do
  code <- mkCode mcode
  mapM_ (write . Syntax.NoReturn) code

-- * Convenience

instance Render (M r a) where
  type Conf (M r a) = Syntax.Conf
  renderM m = do
    env <- ask
    renderM . snd . fst . run env fresh used lib $ m
    where
      State fresh used lib = def

pr :: M r a -> IO ()
pr = TL.putStrLn . render (Syntax.Indent 2)
