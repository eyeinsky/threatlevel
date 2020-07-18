module JS.DSL.Function where

import Prelude
import Data.Functor

import JS.Syntax hiding (Conf)
import JS.DSL.MTL


-- *** Typed

tcall :: (Show a, Args a) => Expr (a, r) -> a -> (Expr r)
tcall f as = TypedFCall f as

-- * Typed functions

-- | Create function from a literal: provide JSM state and reader
funcPrim
  :: Function a
  => (Maybe Name -> [Name] -> Code (Final a) -> Expr (Type a))
  -> State -> a -> (Expr (Type a), State)
funcPrim constr (State fresh used lib) fexp = (constr Nothing args code, s1)
   where
     ((args, code), s1) = run fresh used lib (funcLit fexp)

-- | The 'Function' class turns function literals into typed
-- functions.
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
