module JS.DSL.MTL.Effect
  ( module JS.DSL.Core
  , module JS.DSL.MTL.Effect
  , ask, put
  ) where

import Common.Prelude hiding (next)
import qualified Common.Prelude as P
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HS
import Control.Monad.Writer
import Control.Monad.State hiding (State)
import Control.Monad.Reader

import Render
import JS.Syntax as Syntax
import JS.DSL.Core

-- * Effect

class Monad m => JS m where
  stm :: Syntax.Statement () -> m ()
  freshName :: m Name
  bind :: (Name -> Expr a -> Statement ()) -> Expr a -> m (Expr a)
  execSub :: m a -> m (a, Code_)

  f1 :: forall f . (C1 f, MonadFor f ~ m) => f -> m (Expr ())

  f2 :: forall f . (C2 f m) => FuncConstr () (FunctionType f m) -> f -> m (Expr (FunctionType f m))

execSub_ :: JS m => m a -> m Code_
execSub_ m = execSub m <&> snd

-- * Use C2 as function

type Function = C2
func
  :: forall m f . (JS m, Function f m)
  => FuncConstr () (FunctionType f m) -> f -> m (Expr (FunctionType f m))
func = f2

funcUntyped :: forall m f . (JS m, Function f m) => f -> m RetUntyped
funcUntyped f = coerce <$> c2 f []

-- * Mono

type MonoJS = WriterT Syntax.Code_ (StateT State (Reader Env))
type Result' a = (State, (Syntax.Code_, a))

run :: Env -> Lib -> Used -> Fresh -> MonoJS a -> Result () a
run env lib used fresh m = m
  & runWriterT
  & flip runStateT (State fresh used lib)
  & flip runReaderT env
  & runIdentity

runEmpty :: Syntax.Conf -> MonoJS a -> Result () a
runEmpty env m = run env mempty mempty validIdentifiers m

instance JS MonoJS where
  stm s = tell (pure s)
  freshName = do
    State (Infinite x xs) used lib <- get
    put (State xs used lib) $> Name x
  bind syntax e = do
    name <- freshName
    stm (syntax name e) $> EName name
  execSub m = do
    env <- ask
    (State fresh0 used0 lib0) <- get
    let ((a, w), s) = run env lib0 used0 fresh0 m
    put s $> (a, w)

  f1 f = do
    (names, body) <- c1 f []
    let f = AnonFunc Nothing names body
    fname <- bind Let f
    return fname

  f2 syntax f = do
    (names, body) <- coerce <$> c2 f []
    let f = syntax Nothing names body
    fname <- bind Let f
    return fname

-- * Render

instance Render (MonoJS a) where
  type Conf (MonoJS a) = Syntax.Conf
  renderM m = renderM . resultCode . flip runEmpty m =<< ask

runPretty :: MonoJS a -> Result () a
runPretty = runEmpty (Indent 2)

runMinify :: MonoJS a -> Result () a
runMinify = runEmpty Minify

{- * 1. Get arg list and body

PROBLEMS:
- requires (MonadFor (m a)) ~ m for every a
- requires INCOHERENT
-}

type MonadFor :: Type -> Type -> Type
type family MonadFor f where
  MonadFor (Expr a -> f) = MonadFor f
  MonadFor (m a) = m

class C1 f where c1 :: (JS m, MonadFor f ~ m) => f -> [Name] -> m ([Name], Code_)
instance C1 f => C1 (Expr a -> f) where
  c1 f args = do
    name <- freshName
    let f' = f $ EName name :: f
    c1 f' (name : args)
instance {-# INCOHERENT #-} (MonadFor (m a) ~ m) => C1 (m a) where
  c1 f args = do
    body <- execSub_ f
    return $ (reverse args, body)

{- * 2. Monad in parameter

PROBLEM: requires INCOHERENT
-}

type FunctionType :: Type -> (Type -> Type) -> Type
type family FunctionType f m where
   FunctionType (Expr a -> f) m = a -> FunctionType f m
   FunctionType (m a) m = a

newtype Tagged a b = Tagged b
type RetUntyped = ([Name], Code_)
type Ret f m = Tagged (FunctionType f m) RetUntyped

class Monad m => C2 (f :: Type) (m :: Type -> Type) where
  c2 :: JS m => f -> [Name] -> m (Ret f m)
instance (C2 f m, FunctionType (Expr a -> f) m ~ (a -> FunctionType f m)) => C2 (Expr a -> f) m where
  c2 f args = do
    name <- freshName
    let f' = f $ EName name :: f
    coerce <$> c2 f' (name : args)
instance {-# INCOHERENT #-} (m0 ~ m, Monad m) => C2 (m0 a) m where
  c2 f args = do
    body <- execSub_ f
    return $ Tagged (reverse args, body)

-- | The convert/back combo turns typed function expressions to actual
-- haskell function calls.
type family Convert x where
  Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
  Convert (Expr b) = Expr b
class Back a where
  convert :: [Expr ()] -> a -> Convert a
instance Back (Expr b) => Back (Expr (a -> b)) where
  convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
  convert args f = call f $ reverse args
