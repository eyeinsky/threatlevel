module JS.DSL.MTL.Function where

import Prelude
import Data.Functor
import Data.Default
import Control.Monad.State hiding (State)
import Control.Monad.Reader

import JS.Syntax hiding (Conf)
import qualified JS.Syntax as Syntax
import JS.DSL.MTL.Core

-- * Typed functions

-- | @funcLit@ takes a literal haskell function, feeds it new names
-- used in the function body, and returns the generated names -- to be
-- used in AST of function definition.
class Function a where
   type Type a
   type Final a
   funcLit :: a -> M (Final a) [Name]
instance Function (M r a) where
   type Type (M r a) = r
   type Final (M r a) = r
   funcLit = ($> [])
instance Function (Expr a) where
   type Type (Expr a) = a
   type Final (Expr a) = a
   funcLit e = write (Return $ Cast e) $> []
instance (Function b) => Function (Expr a -> b) where
   type Type (Expr a -> b) = a -> Type b
   type Final (Expr a -> b) = Final b
   funcLit f = do
      arg <- next
      args <- funcLit (f $ EName $ arg)
      return (arg : args)

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

-- * Helpers on top of @Function@ class

-- | Shorthand for clearer type signature. One of @AnonFunc@,
-- @Generator@, @Async@
type FuncConstr a = Maybe Name -> [Name] -> Code (Final a) -> Expr (Type a)

-- | Create function from a literal: provide JSM state and reader
funcPrim
  :: Function a
  => Syntax.Conf -> FuncConstr a -> State -> a -> (Expr (Type a), State)
funcPrim env constr (State fresh used lib) fexp = (constr Nothing args code, s1)
   where
     ((args, code), s1) = run env fresh used lib (funcLit fexp)

-- | Create function, getting state and reader from enclosing monad.
func :: Function f => FuncConstr f -> f -> M parent (Expr (Type f))
func constr f = do
  env <- ask
  (a, s) <- funcPrim env constr <$> get <*> pure f
  put s *> pure a

-- | Return formal arguments and
bla :: Function f => f -> M parent ([Name], Code (Final f))
bla fexp = do
  env <- ask
  State fresh used lib <- get
  let ((args, code), newState) = run env fresh used lib (funcLit fexp)
  put newState
  return (args, code)
