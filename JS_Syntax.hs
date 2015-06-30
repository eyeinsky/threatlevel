module JS_Syntax 
   ( module JS_Syntax
   , UOp(..), BOp(..)
   )
   where

import Prelude2 hiding (True, False)
import Prelude (Float, fromRational, toRational)
import Text.Exts

import Prelude (fromIntegral)
import Data.String
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified JS_Types as JT
import JS_Types (UOp(..), BOp(..))
import Common

type Code a = [ Statement a  ]

-- X=implemented as function, therefore b /= c
data Statement a where
   FuncDefStm  :: Name -> FormalArgs -> Code a -> Statement b
   Var      :: Name -> Statement a
   VarDef   :: Name -> [Name] -> Expr a -> Statement b -- a = [b = c =] expr
   AttrDef  :: Attr -> Expr a -> Statement b -- TODO multiple attrs
   Def      :: Expr a -> Expr b -> Statement c
   BareExpr :: Expr a -> Statement b
   IfElse   :: Expr b -> Code r -> Maybe (Code r) -> Statement r
   For      :: Statement a -> Expr b -> Statement c -> Code d{-X-} -> Statement e
            -- init           cond      post           body
   ForIn    :: Name -> Expr a -> Code b{-X-} -> Statement c

   TryCatch :: Code r -> Code r -> Statement r
   Return   :: Expr a -> Statement a




data Expr a where
   Cast      :: Expr a              -> Expr b
   Par       :: Expr a              -> Expr a
   EName     :: Name                -> Expr a -- name
   EAttr     :: Attr                -> Expr a -- expr.name
   Arr       :: Expr a -> Expr b    -> Expr c -- expr[expr]

   -- untyped
   ULit      :: ULiteral            -> Expr a 
   Op        :: OpExpr  a           -> Expr a -- expr `op` expr

   -- typed
   Literal   :: Literal a => a      -> Expr a
   BOp       :: E (Proxy o) => BOpExpr t o -> Expr t -- expr + expr
   UOp       :: UOpExpr t o         -> Expr t -- expr + expr

   -- untyped
   FuncCall  :: Expr a -> [Expr b]  -> Expr c -- TODO expr(*expr)

   -- typed
   FuncDef    :: [Expr a] -> Code b -> Expr c
   TypedFDef  :: Args a => a -> Code b -> Expr c
   TypedFCall :: (Show a, Args a) => Expr (a, r) -> a -> Expr r

   Ternary   :: Expr JT.Bool -> Expr a -> Expr a -> Expr a
   Null      :: Expr a
   Undefined :: Expr a
   Raw       :: Text -> Expr a -- inject raw js code


-- ** Operators 

-- *** Untyped

data OpExpr a where
   OpBinary :: BOp -> Expr a -> Expr b -> OpExpr c
   OpUnary :: UOp -> Expr a -> OpExpr b

-- *** Typed operators

data BOpExpr t o where
   BOE :: JT.O t o => Proxy o -> Expr t -> Expr t -> BOpExpr (JT.D t o) o
data UOpExpr t o where
   UOE :: JT.U t o => Expr t -> UOpExpr t o

instance E (Proxy o) => E (BOpExpr a o) where
   ev (BOE p a b) = ev a <> ev p <> ev b
instance E (Proxy o) => E (UOpExpr a o) where
   ev (UOE a) = ev (Proxy :: Proxy o) <> ev a



data FormalArgs = FA [Text]

data Name = Name Text

data Attr = forall a. Attr (Expr a) Name



-- * AST to JavaScript text

instance E (Code a) where
   ev li = T.intercalate ";\n" $ map ev li
instance E (Statement a) where
   ev stm = case stm of
      AttrDef attr exp -> ev attr =: ev exp
      Var name -> "var " <> ev name
      VarDef n ns exp -> ("var " <> ev n) =: (T.intercalate eq (map ev ns) <> ev exp)
      Def e1 e2 -> ev e1 =: ev e2
      BareExpr expr -> ev expr
      
      For init cond post conts -> "for"
         <> (par . unsemi) [ev init, ev cond, ev post]
         <> curly (ev conts)

      ForIn name expr code -> "for"
         <> (par $ (ev . Var $ name) <> " in " <> ev expr)
         <> (curly $ ev code <> ";")

      IfElse c tc mec -> "if" <> par (ev c)
         <> curly (ev tc) <> maybe "" (("else" <>) . curly . ev) mec
      Return expr -> "return " <> ev expr
      where 
         a =: b = a <> " = " <> b
         eq = " = "

instance E (Expr a) where
   ev expr = case expr of
      EName name -> ev name
      Arr arrExpr ixExpr -> ev arrExpr <> ang (ev ixExpr)
      EAttr attr -> ev attr
      Literal lit -> ev lit
      ULit lit -> ev lit
      Op opExpr -> ev opExpr
      BOp expr -> ev expr
      -- UOp expr -> ev expr
     
      FuncCall name exprs -> ev name <> unargs exprs

      FuncDef as code -> "function" <> unargs as <> uncode code
      TypedFCall f as -> par (ev f) <> unargs (args as)
      -- DELME TypedFDef as code -> "function" <> unargs (args as) <> uncode code

      Ternary b t f -> par (ev b <> "?" <> ev t <> ":" <> ev f)

      Null -> "null"
      Undefined -> "undefined"

      -- meta
      Par expr -> par $ ev expr -- parenthesis around
      Raw stm -> stm -- raw js text
      Cast e -> ev e -- change type

      where
         unargs = par . uncomma . map ev
         uncode code = curly (ev code <> ";")

col (k, v) = either ev ev k <> ": " <> ev v


instance E (OpExpr a) where
   ev o = case o of
      OpBinary op e1 e2 -> ev e1 <> ev op <> ev e2
      OpUnary op e -> error "web:JS_Syntax.hs:E OpUnary"
instance E BOp where
   ev op = case op of
      Minus -> "-"
      Plus -> "+"
      Mult -> "*"
      Div -> "/"
      Eq   -> "=="
      NEq  -> "!="
      EEq  -> "==="
      NEEq -> "!=="
      And -> "&&"
      Or  -> "||"
      Lt  -> "<"
      Gt  -> ">"
      LEt  -> "<="
      GEt  -> ">="
instance E Attr where
   ev (Attr exp name) = sur "(" ")" (ev exp) <> "." <> ev name


instance E Name where
   ev (Name s) = s


-- ** Literals

-- *** Typed Literals

instance E JT.Bool   where ev (JT.Bool a) = T.toLower $ tshow a
instance E JT.Number where ev (JT.Number a) = tshow a
instance E JT.NumberI where ev (JT.NumberI a) = tshow a
instance E JT.String where ev (JT.String a) = q'' a
instance E JT.Object where ev (JT.Object) = u
instance E JT.Regex where ev (JT.Regex pat mod) = sur "/" "/" pat <> mod

class    E a => Literal a where
instance Literal JT.Bool where
instance Literal JT.Number where
instance Literal JT.NumberI where
instance Literal JT.String where
instance Literal JT.Object where
instance Literal JT.Regex where
-- instance E a => Literal (JT.Array a) where

class    ToLiteral a        where
   type Dest a
   lit :: a -> Expr (Dest a)
instance ToLiteral Bool where
   type Dest Bool = JT.Bool
   lit v = Literal $ JT.Bool v
instance ToLiteral T.Text where
   type Dest T.Text = JT.String
   lit v = Literal $ JT.String v
instance ToLiteral TL.Text  where
   type Dest TL.Text = JT.String
   lit v = lit $ TL.toStrict v
instance ToLiteral String where
   type Dest String = JT.String
   lit v = lit $ T.pack v
instance ToLiteral Int where
   type Dest Int = JT.NumberI
   lit v = Literal $ JT.NumberI $ toInteger v
instance ToLiteral Integer where
   type Dest Integer = JT.NumberI
   lit v = Literal $ JT.NumberI v
instance ToLiteral Double where
   type Dest Double = JT.Number
   lit v = Literal $ JT.Number v
instance ToLiteral Float where
   type Dest Float = JT.Number
   lit v = Literal $ JT.Number $ fromRational $ toRational v


--- *** Untyped literals

data ULiteral
   = ULString T.Text
   | ULDouble Double
   | ULInteger Integer
   | ULBool Bool
   | ULArray [Expr ()]
   | ULObject [(Either Name (Expr ()), Expr ())]

instance E ULiteral where
   ev ul = case ul of
      ULString text -> q'' text
      ULDouble dbl -> tshow dbl
      ULInteger i -> tshow i
      ULBool b -> T.toLower $ tshow b
      ULArray li -> ang $ uncomma $ map ev li
      ULObject obj -> curly $ uncomma . map f $ obj
         where f (e,expr) = either ev ev e <> ":" <> ev expr 

class    ToULiteral a       where uliteral :: a -> ULiteral
instance ToULiteral Int     where uliteral = ULInteger . toInteger
instance ToULiteral Integer where uliteral = ULInteger 
instance ToULiteral Bool    where uliteral = ULBool
instance ToULiteral T.Text  where uliteral = ULString
instance ToULiteral TL.Text where uliteral = uliteral . TL.toStrict
instance ToULiteral String  where uliteral = uliteral . T.pack
instance ToULiteral [ Expr a ] where
   uliteral = ULArray . map Cast
instance ToULiteral a => ToULiteral [(a, Expr b)] where
   uliteral li = ULObject $ map f li
      where f (a,b) = (Right . ulit $ a, Cast b)

ulit = ULit . uliteral :: ToULiteral a => a -> Expr b


attr :: Expr a -> Name -> Expr b
attr base attname = EAttr $ Attr base attname

instance IsString Name where
   fromString s = Name $ T.pack s
instance IsString (Expr a) where
   fromString s = EName $ fromString s 

cast :: Expr a -> Expr ()
cast x = Cast x


-- transform (Expr a, (Expr b, .. to 
class Args a      where args :: a   -> [ Expr () ]
instance Args (Expr a, ())  where args (e, ()) = [ Cast e  ]
instance Args (b, c)
   => Args (Expr a, (b, c)) where args (e, t)  = Cast e : args t
