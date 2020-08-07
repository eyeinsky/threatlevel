module JS.Syntax.Types where

import X.Prelude hiding (True, False, Empty, unwords, unlines, replicate)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL


type Code a = [Statement a]

data Statement a where
   FuncDefStm :: Name -> [Name] -> Code a -> Statement b
   Var      :: Name -> Statement a
   VarDef   :: Name -> Expr a -> Statement b
   BareExpr :: Expr a -> Statement b
   IfElse   :: Expr b -> Code r -> Maybe (Code r) -> Statement r

   For      :: Statement a -> Expr b -> Statement c -> Code d{-X-} -> Statement e -- NOTE: X = implemented as function, therefore b /= c
            -- init           cond      post           body
   ForIn    :: Name -> Expr a -> Code b{-X-} -> Statement c
   ForAwait :: Name -> Expr a -> Code b -> Statement c
   While    :: Expr a -> Code b{-X-} -> Statement c
   Continue :: Maybe Name -> Statement a
   Break :: Maybe Name -> Statement a

   TryCatchFinally :: Code r -> [(Name, Code r)] -> Maybe (Code r) -> Statement r
   Throw    :: Expr a -> Statement r

   Return   :: Expr a -> Statement a
   Empty    :: Statement a

   Let      :: Name -> Expr a -> Statement b
   Const    :: Name -> Expr a -> Statement b

   Switch   :: Expr a -> ([(Expr a, Code r)]) -> Maybe (Code r) -> Statement r

data Expr a where
   Assign    :: Expr a -> Expr b    -> Expr b
   Cast      :: Expr a              -> Expr b
   Raw       :: TL.Text             -> Expr a -- inject raw js code
   Par       :: Expr a              -> Expr a -- parenthesis
   EName     :: Name                -> Expr a -- name
   EAttr     :: Attr                -> Expr a -- expr.name
   Arr       :: Expr a -> Expr b    -> Expr c -- expr[expr]
   In        :: Expr a -> Expr b    -> Expr Bool

   -- untyped
   Lit      :: Literal              -> Expr a -- 1
   Op        :: OpExpr  a           -> Expr a -- expr + expr

   -- untyped
   FuncCall  :: Expr a -> [Expr b]  -> Expr c -- func(*expr)
   -- typed
   AnonFunc :: Maybe Name -> [Name] -> Code b -> Expr c
   Generator :: Maybe Name -> [Name] -> Code b -> Expr c
   Async :: Maybe Name -> [Name] -> Code b -> Expr c

   -- ^ function maybeName(exprs) {code}
   -- TypedFDef  :: Args a => a -> Code b -> Expr c
   TypedFCall :: (Show a, Args a) => Expr (a, r) -> a -> Expr r

   Ternary   :: Expr Bool -> Expr a -> Expr a -> Expr a
   Null      :: Expr a
   Undefined :: Expr a

   Yield     :: Expr a -> Expr b
   YieldDelegate :: Expr a -> Expr b
   Await     :: Expr a -> Expr b

   New       :: Expr a -> Expr b

data FormalArgs = FA [TL.Text]
data Name = Name { getName :: TS.Text }

instance IsString Name where
  fromString = Name . TS.pack

data Attr = forall a. Attr (Expr a) Name

-- ** Operators

-- * Operators

data UOp
  = UMinus | UPlus
  | TypeOf
  | Not
  -- | Increment post/pre | Decrement post/pre

data BOp
   = Minus | Plus | Mult | Div | Modulus
   | Eq  | NEq | EEq | NEEq
   | And | Or
   | Gt  | Lt  | GEt | LEt
   | Instanceof

data OpExpr a where
   OpBinary :: BOp -> Expr a -> Expr b -> OpExpr c
   OpUnary :: UOp -> Expr a -> OpExpr b

data Literal
   = String TS.Text
   | RegExp TS.Text TS.Text
   | Double Double
   | Integer Integer
   | Bool Bool
   | Array [Expr ()]
   | Object [(Either Name (Expr ()), Expr ())]

attr :: Expr a -> Name -> Expr b
attr base attname = EAttr $ Attr base attname

cast :: Expr a -> Expr ()
cast x = Cast x


-- transform (Expr a, (Expr b, .. to
class Args a      where args :: a   -> [ Expr () ]
instance Args (Expr a, ())  where args (e, ()) = [ Cast e  ]
instance Args (b, c)
   => Args (Expr a, (b, c)) where args (e, t)  = Cast e : args t

-- * Helpers

-- * Non-monadic helpers

ex txt = EName $ Name txt

(!.) :: Expr a -> TS.Text -> Expr b
(!.) expr attr = EAttr $ Attr (Cast expr) (Name attr)


(.!) expr key  = Arr expr key

call :: Expr a -> [Expr b] -> Expr c
call  f as = FuncCall f as

call0 f = FuncCall f []
call1 f a = FuncCall f [a]
