module JS.DSL.MTL.Mono
  ( module JS.DSL.MTL.Mono
  , module JS.DSL.MTL.Effect
  , module JS.DSL.Core
  ) where

import Common.Prelude
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any)
import Control.Monad.State

import Render

import JS.Syntax as Syntax
import JS.DSL.Core
import JS.DSL.MTL.Effect

-- * MTL base

stmBase tell = tell . pure

freshNameBase get put = do
  State (Infinite x xs) used lib <- get
  put (State xs used lib) $> Name x

bindBase syntax e = do
  name <- freshName
  stm (syntax name e) $> EName name

execSubBase ask get put run m = do
  env <- ask
  state0 <- get
  let ((a, w), s) = run env state0 m
  put s $> (a, w)

f2Base bind syntax f =
  bind Let . uncurry (syntax Nothing) =<< coerce <$> c2 f []

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

runEmpty :: Syntax.Conf -> MonoJS a -> Result () a
runEmpty env m = run env (State validIdentifiers mempty mempty) m

instance JS MonoJS where
  stm = stmBase tell
  freshName = freshNameBase get put
  bind = bindBase
  execSub = execSubBase ask get put run

  f1 f = bind Let . uncurry (AnonFunc Nothing) =<< c1 f []
  f2 syntax f = f2Base bind syntax f
  f3 f = c3 f []

-- * Render

instance Render (MonoJS a) where
  type Conf (MonoJS a) = Syntax.Conf
  renderM m = renderM . resultCode . flip runEmpty m =<< ask

runPretty :: MonoJS a -> Result () a
runPretty = runEmpty (Indent 2)

runMinify :: MonoJS a -> Result () a
runMinify = runEmpty Minify

-- * 3.2 Use polykinded m

type Lift :: () -> (Type -> Type) -> Type -> Type
newtype Lift x m a = Lift (m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- data MonoJSRaw x a = MonoJSRaw (WriterT Syntax.Code_ (StateT State (Reader Env))) a
-- SampleImplRaw (x :: ()) a =

type MonoJSRaw x = Lift x (WriterT Writer_ (StateT State_ (Reader Reader_)))
type MonoJS'' = MonoJSRaw '()

run' :: Env -> Lib -> Used -> Fresh -> MonoJS'' a -> Result () a
run' env lib used fresh m = m
  & coerce
  & runWriterT
  & flip runStateT (State fresh used lib)
  & flip runReaderT env
  & runIdentity

runEmpty' :: Syntax.Conf -> MonoJS'' a -> Result () a
runEmpty' env m = run' env mempty mempty validIdentifiers m

instance JS MonoJS'' where
  stm s = lift $ tell (pure s)
  freshName = lift $ do
    State (Infinite x xs) used lib <- get
    put (State xs used lib) $> Name x
  bind syntax e = lift $ do
    name <- freshName
    stm (syntax name e) $> EName name
  execSub m = lift $ do
    env <- ask
    (State fresh0 used0 lib0) <- get
    let ((a, w), s) = run' env lib0 used0 fresh0 m
    put s $> (a, w)

  -- f1 f = bind Let . uncurry (AnonFunc Nothing) =<< c1 f []
  -- f2 syntax f = bind Let . uncurry (syntax Nothing) =<< coerce <$> c2 f []
  f3 f = c3 f []


-- * Polykinded type family

type F :: k -> Type
type family F a where
  F (a -> b) = ()
  F Maybe = ()

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
