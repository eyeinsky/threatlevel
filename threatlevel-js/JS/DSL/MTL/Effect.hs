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

class (Monad m) => JS m where
  stm :: Syntax.Statement () -> m ()
  freshName :: m Name
  bind :: (Name -> Expr a -> Statement ()) -> Expr a -> m (Expr a)
  execSub :: m a -> m (a, Code_)

  f1 :: forall f . (C1 f, MonadFor f ~ m) => f -> m (Expr ())

  f2 :: forall f . (C2 f m) => FuncConstr () (FunctionType f m) -> f -> m (Expr (FunctionType f m))

  f3 :: forall f . (C3 f m) => f -> m RetUntyped

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

  f1 f = bind Let . uncurry (AnonFunc Nothing) =<< c1 f []
  f2 syntax f = bind Let . uncurry (syntax Nothing) =<< coerce <$> c2 f []
  -- f3 f = do
  --   c3 f
    -- g <- c3 f
    -- let args = toStrings g
    --     body = undefined
    -- bind Let $ syntax Nothing args [Empty]

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

{- * 3.1 Polykinded m, base takes an extra ()

-}

type C3 :: Type -> (Type -> Type) -> Constraint
class Monad m => C3 f m where
  c3 :: JS m => f -> [Name] -> m RetUntyped
instance (C3 f m) => C3 (Expr a -> f) m where
  c3 f args = do
    name <- freshName
    let f' = f $ EName name :: f
    coerce <$> c3 f' (name : args)
instance (p ~ '(), p' ~ p, m' ~ m, Monad (m '())) => C3 (m' p' a) (m p) where
  c3 f args = do
    body <- execSub_ f
    return (reverse args, body)

-- * Convert typed expressions to functions on expressions

-- | Example: @Expr (a -> b -> c)@ to @Expr a -> Expr b -> Expr c@

type family Convert x where
  Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
  Convert (Expr b) = Expr b
class Back a where
  convert :: [Expr ()] -> a -> Convert a
instance Back (Expr b) => Back (Expr (a -> b)) where
  convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
  convert args f = call f $ reverse args

{- * 3. Use marked base case

-}

-- testf :: a -> b -> c -> d -> Identity Int
testf :: Monad m => a -> b -> c -> d -> m ()
testf a b c d = return undefined

testAnyGProxy :: Monad m => G (a -> b -> c -> d -> m ())
testAnyGProxy = anyGProxy (fproxy testf)

-- testArity :: Int
-- testArity = arity testf

data Some (f :: k -> Type) where
  Some :: f a -> Some f


-- class AnyG f where
--   anyG :: G (ToG f)
-- instance (AnyG b, ToG (a -> b) ~ f) => AnyG f where
--   anyG = Rec (anyG :: G (ToG f))
-- instance {-# OVERLAPPABLE #-} AnyG a where
--   anyG = End

-- toG :: Proxy f -> G f -> Some G
-- toG _ g = case g of
--   Rec some -> undefined
--   Rec some -> undefined


-- * G class

data G :: Type -> Type where
  GRec :: G b -> G (a -> b)
  GEnd :: G a

class AnyG f where
  anyG :: G f
  anyGProxy :: Proxy f -> G f
instance {-# OVERLAPPING #-} AnyG b => AnyG (a -> b) where
  anyG = GRec anyG
  anyGProxy _ = GRec (anyGProxy Proxy)
instance {-# INCOHERENT #-} AnyG a where
  anyG = GEnd
  anyGProxy _ = GEnd


-- class AnyG f => Arity f where arity :: Proxy f -> Int
-- class Arity f where arity :: Proxy f -> Int


-- * Polykinded type family

-- type X :: (Type -> Type -> Type) -> Type
type X :: k -> Type
type family X a where
  X (->) = Int
  X Maybe = ()


-- * Minimal 2

class C (f :: Type) where
  str :: f -> String
instance C (a -> b) where
  str _ = "rec"
-- instance C ((m :: Type -> Type) a) where
--   str _ = "base"
instance C a where --
  str _ = "base"

poly :: forall m . Monad m => m ()
poly = return ()

mono :: IO ()
mono = return ()

-- main = str poly




-- main :: IO ()
-- main = do
--   print $ str poly
--   return ()

-- printStr :: C a m => a -> IO ()
-- printStr a = do
--   a' <- str a
--   putStrLn a'

-- main :: IO ()
-- main = printStr mono

-- * Arity/G depth

gDepth :: forall f . G f -> Int
gDepth g = case g of
  GRec b -> 1 + gDepth b
  GEnd{} -> 0

arity :: forall f . AnyG f => f -> Int
arity _ = gDepth (anyG :: G f)

-- * Helpers

liftReturn :: f -> Proxy (LiftReturn Id f)
liftReturn _ = Proxy

fproxy :: f -> Proxy f
fproxy _ = Proxy

newtype Id a = Id (forall x . x)
type LiftReturn :: (Type -> Type) -> Type -> Type
type family LiftReturn g f where
  LiftReturn g (a -> b) = a -> LiftReturn g b
  LiftReturn g a = g a

type family Erase f where
  Erase (a -> b) = a -> Erase b
  Erase a = ()
