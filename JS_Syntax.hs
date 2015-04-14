module JS_Syntax where

import Prelude2 hiding (True, False)
import Text.Exts

import Prelude (fromIntegral)
import Data.String
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- import Common

type S = T.Text
type Code = [Statement]

data Statement
   = FuncDef Name FormalArgs Code
   | Var Name
   | VarDef Name [Name] Expr -- a = [b = c =] expr
   | AttrDef Attr Expr -- TODO multiple attrs
   | Def Expr Expr
   | BareExpr Expr
   | IfElse Expr Code (Maybe Code)
   | For Statement Expr  Statement  Code
      -- init      cond  post       body
   | ForIn Name Expr Code

   | TryCatch Code Code
   | Return Expr

data Expr where
   Par       :: Expr           -> Expr
   EName     :: Name           -> Expr -- name
   EAttr     :: Attr           -> Expr -- expr.name
   Arr       :: Expr -> Expr   -> Expr -- expr[expr]
   Literal   :: Literal        -> Expr -- lit
   Op        :: OpExpr         -> Expr -- expr + expr
   FuncExpr  :: Code           -> Expr -- fuction() { code }
   FuncCall  :: Expr -> [Expr] -> Expr -- expr(*expr)
   Ternary   :: Expr -> Expr -> Expr -> Expr
   Regex     :: S -> S         -> Expr

   Null      :: Expr
   Undefined :: Expr
   True      :: Expr
   False     :: Expr

   Raw       :: S -> Expr -- inject raw js code



data OpExpr
   = OpBinary BOp Expr Expr
   | OpUnary UOp Expr

data UOp = UMinus | UPlus | TypeOf

data BOp
   = BMinus | BPlus | Mult | Div
   | Eq | NEq | EEq | NEEq
   | And | Or
   | Gt | Lt
   | GEt | LEt

data Literal
   = String S
   | Double Double
   | Int    Integer
   | Array  [ Expr ]
   | Object [ (Either Name Expr, Expr) ]

data FormalArgs = FA [S]

data Name = Name S

data Attr = Attr Expr Name


deriving instance Show Statement
deriving instance Show Expr
deriving instance Show OpExpr
deriving instance Show UOp
deriving instance Show BOp
deriving instance Show Literal
-- deriving instance Show PropDef
deriving instance Show FormalArgs
deriving instance Show Name
deriving instance Show Attr




-- * AST to JavaScript text

class E a where
   ev :: a -> T.Text

instance E Code where
   ev li = T.intercalate ";\n" $ map ev li
instance E Statement where
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
instance E Expr where
   ev expr = case expr of
      Par   expr -> par $ ev expr
      EName name -> ev name
      Arr arrExpr ixExpr -> ev arrExpr <> ang (ev ixExpr)
      EAttr attr -> ev attr
      Literal lit -> ev lit
      Op opExpr -> ev opExpr
      FuncExpr code -> "function() {\n" <> ev code <> ";\n}"
      FuncCall name exprs -> ev name <> par (uncomma $ map ev exprs)
      Ternary b t f -> par (ev b <> "?" <> ev t <> ":" <> ev f)
      Regex pat mod -> sur "/" "/" pat <> mod

      Null -> "null"
      Undefined -> "undefined"
      True -> "true"
      False -> "false"

      Raw stm -> stm


col (k, v) = either ev ev k <> ": " <> ev v


instance E OpExpr where
   ev o = case o of
      OpBinary op e1 e2 -> ev e1 <> ev op <> ev e2
      OpUnary op e -> error "web:JS_Syntax.hs:E OpUnary"
instance E BOp where
   ev op = case op of
      BMinus -> "-"
      BPlus -> "+"
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
instance E Literal where
   ev x = case x of
      String s -> q'' s
      Double s -> tshow s
      Int    s -> tshow s
      Array  li -> ang $ uncomma $ map ev li
      Object li -> curly $ uncomma $ map col li
   -- ev (Object s) = show s



-- * AST creation convenience

class    ToLit a where toLit :: a -> Literal
instance ToLit Int      where toLit v = Int $ toInteger v
instance ToLit Integer  where toLit v = Int v
instance ToLit Double   where toLit v = Double v
instance ToLit S        where toLit v = String v
instance ToLit String   where toLit v = String $ T.pack v
instance ToLit TL.Text  where toLit v = String $ TL.toStrict v
instance ToLit a => ToLit [(a, Expr)] where
   toLit li = Object $ map f li
      where f (a,b) = (Right . lit $ a, b)

instance ToLit [ Expr ] where
   toLit = Array

lit :: ToLit a => a -> Expr
lit = Literal . toLit

attr :: Expr -> Name -> Expr
attr base attname = EAttr $ Attr base attname

instance IsString Name where
   fromString s = Name $ T.pack s
instance IsString Expr where
   fromString s = EName $ fromString s 

plus a b = Op $ OpBinary BPlus a b
