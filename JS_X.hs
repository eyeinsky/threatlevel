module JS_X where

import Prelude2

import JS_Monad
import qualified JS_Types as JT

data Func a r
data Term t

data Var a where
   Func :: a -> Var (Func a r)
   Term :: a -> Var (Term a)


type family Ret a r

type family F a 
type instance F (a, END) = (Expr a, END)
type instance F (a, (b, c)) = (Expr a, F (b, c))
type END = ()

-- the mock function
type A1 = JT.Bool
type A2 = JT.String
type A3 = JT.Number
type R1 = JT.Object
type FA = (A1, (A2, (A3, END))) -- formal
type ManArgs = (Expr A1, (Expr A2, (Expr A3, END)))
type AutoArgs = F FA


f :: Expr (Func FA R1)
f = u
as :: {- ManArgs ~ AutoArgs => -} ManArgs
as = u
t1 = app f as 

app :: Expr (Func a r) -> F a -> M (Expr r)
app f as = u
{- ^
   'call' equivalent for typed function application
   'call' usage: call fexpr [ expr1, expr2, .. ]
   -}



data Refl a b where
   Refl :: a -> a -> Refl a a


