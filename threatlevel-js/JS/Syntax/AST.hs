module JS.Syntax.AST where

import Prelude
import Data.Void
import Data.String
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL


type Code a = [Statement a]
type Code_ = Code ()

data Statement a where
   FuncDec  :: Name -> [Name] -> Code a -> Statement b
   -- ^ Function declaration, apart from
   -- function-expression-as-expression-statement
   -- https://2ality.com/2012/09/expressions-vs-statements.html
   VarDec   :: Name -> Statement a
   VarDecDef:: Name -> Expr a -> Statement b
   BareExpr :: Expr a -> Statement b
   IfElse   :: Expr b -> Code r -> Maybe (Code r) -> Statement r

   For      :: Statement a -> Expr b -> Statement c -> Code d{-X-} -> Statement e -- NOTE: X = implemented as function, therefore b /= c
            -- init           cond      post           body
   ForIn    :: Name -> Expr a -> Code b{-X-} -> Statement c
   ForOf    :: Name -> Expr a -> Code b{-X-} -> Statement c -- todo: discern const/let
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

   Class    :: Name -> Maybe Name -> [ClassBodyPart] -> Statement r

   -- | Provide a typed way to embed @M Void a@ within any return type
   -- context. With Void we know that the code can't return.
   NoReturn   :: Statement Void -> Statement b

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

   -- * Function expressions
   -- | @function maybeName(name) {code}@
   Func :: Maybe Name -> [Name] -> Code b -> Expr c
   -- | @(names) => {code}@
   FuncArrow :: [Name] -> Code b -> Expr c
   -- | @function *maybeName(name) {code}@
   Generator :: Maybe Name -> [Name] -> Code b -> Expr c
   -- | @async function maybeName(names) {code}@
   Async :: Maybe Name -> [Name] -> Code b -> Expr c
   -- | @async (names) => {code}@
   AsyncArrow :: [Name] -> Code b -> Expr c

   Ternary   :: Expr Bool -> Expr a -> Expr a -> Expr a
   Null      :: Expr a
   Undefined :: Expr a

   Yield     :: Expr a -> Expr b
   YieldDelegate :: Expr a -> Expr b
   Await     :: Expr a -> Expr b

   -- * Classes
   -- | @class extends Other {...}@
   ClassExpr :: Maybe Name -> [ClassBodyPart] -> Expr r
   -- | @new returnsClass(...)@
   New       :: Expr a -> Expr b

type FormalArgs = [Name]
newtype Name = Name TS.Text
  deriving stock Show

data PossiblyComputedName
  = Computed (Expr ())
  | Regular Name

instance IsString Name where
  fromString = Name . TS.pack

data Attr = forall a. Attr (Expr a) Name

-- * Class

data ClassBodyPart
  = forall a. ClassBodyMethod ClassBodyMethodType (Code a)
  | forall a. ClassBodyField ClassBodyFieldType (Expr a)

data ClassBodyMethodType
  = Constructor FormalArgs
  | InstanceMethod Name FormalArgs
  | StaticMethod Name FormalArgs
  | Getter Name -- ^ just name
  | StaticGetter Name -- ^ just name but class-global
  | Setter Name Name -- ^ name and single argument

data ClassBodyFieldType
  = Instance Name
  | Static Name
  | Private Name

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

ex :: TS.Text -> Expr a
ex txt = EName $ Name txt

(!.) :: Expr a -> TS.Text -> Expr b
(!.) expr attr = EAttr $ Attr (Cast expr) (Name attr)

(.!) :: Expr a -> Expr b -> Expr c
(.!) expr key  = Arr expr key

call :: Expr a -> [Expr b] -> Expr c
call  f as = FuncCall f as

call0 :: Expr a -> Expr c
call0 f = FuncCall f []

call1 :: Expr a -> Expr b -> Expr c
call1 f a = FuncCall f [a]

type FuncConstr a b = Maybe Name -> [Name] -> Code a -> Expr b

-- | Common type for @VarDef@, @Let@ and @Const@
type VarDecl a b = Name -> Expr a -> Statement b
