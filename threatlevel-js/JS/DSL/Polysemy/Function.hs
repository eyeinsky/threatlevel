{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Function where

import Common.Prelude hiding (next)
import JS.Syntax as Syntax
import JS.DSL.Polysemy.Core

import Polysemy hiding (Final, run)
import Polysemy.Writer qualified as Polysemy
import Polysemy.State qualified as Polysemy

-- * Typed functions

-- | Shorthand for clearer type signature. One of @AnonFunc@,
-- @Generator@, @Async@
type FuncConstr' f = FuncConstr (ReturnType f) (FunctionType f)

-- | @funcLit@ takes a literal haskell function, feeds it new names
-- used in the function body, and returns the generated names -- to be
-- used in AST of function definition.
class Function a where
   type FunctionType a
   type ReturnType a
   funcLit :: Member (JS r) r => a -> Sem r [Name]


-- instance Function (M r a) where
--    type FunctionType (M r a) = r
--    type ReturnType (M r a) = r
--    funcLit = ($> [])
-- instance Function (Expr a) where
--    type FunctionType (Expr a) = a
--    type ReturnType (Expr a) = a
--    funcLit :: Expr a -> M a [Name]
--    funcLit e = write @a (Return $ Cast e) $> []
-- instance (Function b) => Function (Expr a -> b) where
--    type FunctionType (Expr a -> b) = a -> FunctionType b
--    type ReturnType (Expr a -> b) = ReturnType b
--    funcLit f = do
--       arg <- next
--       args <- funcLit (f $ EName $ arg)
--       return (arg : args)

-- -- | The convert/back combo turns typed function expressions to actual
-- -- haskell function calls.
-- type family Convert x where
--   Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
--   Convert (Expr b) = Expr b
-- class Back a where
--   convert :: [Expr ()] -> a -> Convert a
-- instance Back (Expr b) => Back (Expr (a -> b)) where
--   convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
-- instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
--   convert args f = call f $ reverse args

-- * Helpers on top of @Function@ class


-- -- | Create function, getting state and reader from enclosing monad.
func
  :: forall r f . (Member (JS r) r, Function f)
  => FuncConstr' f -> f -> Sem r (Expr (FunctionType f))
func constr f = do
  let fbody = funcLit @f @r f :: Sem r [Name]
  -- ^ I think the @e@ means both current scope and function need to
  -- have the same return type?

--  defineFunction fbody

--   env <- askEnv
--   state <- getState
--   let (a, new) = funcPrim env constr state f
--   putState new *> pure a
--   where
--     -- | Create function from a literal: provide JSM state and reader
--     funcPrim
--       :: Function a
--       => Syntax.Conf -> FuncConstr a -> State -> a -> (Expr (FunctionType a), State)
--     funcPrim env constr (State fresh used lib) fexp = (constr Nothing args code, s1)
--        where
--          ((args, code), s1) = run env lib used fresh (funcLit fexp)
  undefined

-- | Return formal arguments and
-- bla :: forall r m f . Function f => () -> Poly r m ([Name], Code (ReturnType f))
-- bla () = do
--  env <- askEnv
--  State fresh used lib <- getState
--  let ((args, code), new) = run env lib used fresh undefined -- (funcLit fexp)
--  putState new
  -- return (args, code)
--  undefined
