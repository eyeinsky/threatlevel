{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Prelude
import Data.Kind
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

-- I'd like to use literal haskell functions in a dsl as follows

program :: forall m . DSL m => m ()
program = do
  stm $ Stm (Var "a")
  -- this way:
  f <- fun $ \(a :: Expr a) (b :: Expr b) -> do
    -- function body is of the same monad `m` as the one in top level
    stm $ Stm (Var "b")
  stm $ Stm $ Apply f (Var "c")
  pure ()

{- .. and turn the above into code (`[Stm]`).

To make the functions work there is a helper class `Fun` (code below)
that saturates the function with fresh variables until it reaches
function body (which should run in the same `m` as top-level code),
then returns both the list of arguments and body itself so that they
could be made into function syntax[1]. There is a recursive case to
generate arguments[2] and a base case to evaluate body to `[Stm]`.[3]

The worrisome instance is the base case[4], which currently requires
`{-# INCOHERENT #-}`, because ghc can't choose a Fun instance because
function's body in the `program` is ambiguous (while it should just be
whatever `m` is at top level code).

So the question is: is there a way to force the body to always use the
same m as in top level and not require incoherence?

A stub AST, an MTL-style DSL "effect" and a sample implementation:
-}

-- | An AST
data Expr a
  = Var String
  | Apply String (Expr a)
  | Function String [String] [Stm]
data Stm = Stm (Expr ())

-- | The "effect"
class Monad m => DSL m where
  freshName :: m String -- generate fresh variable name
  stm :: Stm -> m () -- emit statement
  toAST :: m a -> m [Stm] -- turn code `m a` into [Stm] (without emitting it)
  fun :: Fun f m => f -> m String -- emit function f, return its name

-- | Helper class to convert literal haskell functions to the dsl
class Fun f m where
  mkFun :: DSL m => f -> [String] -> m ([String], [Stm])
instance Fun f m => Fun (Expr a -> f) m where
  mkFun f acc = do
    name <- freshName -- [2]
    mkFun (f $ Var name) (name : acc)
instance {-# INCOHERENT #-} (m0 ~ m) => Fun (m0 a) m where -- [4]
  mkFun m args = do
    fname <- freshName
    body <- toAST m -- [3]
    return (args, body)

-- | A sample implementation
instance DSL (StateT Int (Writer [Stm])) where
  freshName = do
    n <- get
    put $ n + 1
    return $ "var" <> show n
  toAST m = do
    state0 <- get
    let ((_, state1), w) = run m state0
    put state1
    return w
  fun f = do
    (args, body) <- mkFun f []
    name <- freshName
    stm $ Stm $ Function name args body -- [1]
    return name

run :: StateT Int (Writer [Stm]) a -> Int -> ((a, Int), [Stm])
run m s = runWriter $ runStateT m s
