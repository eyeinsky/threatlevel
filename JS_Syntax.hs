module JS_Syntax where

import Prelude2 hiding (True, False)
import Text.Exts

import Prelude (fromIntegral)
import Data.String
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified JS_Types as JT

type Text = T.Text
type Code a = [Statement a]
type Code' = [Statement ()]

data Statement a
   = FuncDef Name FormalArgs (Code a)
   | Var Name
   | VarDef Name [Name] (Expr a) -- a = [b = c =] expr
   | AttrDef Attr (Expr a) -- TODO multiple attrs
   | Def (Expr a) (Expr a)
   | BareExpr (Expr a)
   | IfElse (Expr a) (Code a) (Maybe (Code a))
   | For (Statement ()) (Expr a) (Statement ()) (Code a)
      -- init           cond     post           body
   | ForIn Name (Expr a) (Code a)

   | TryCatch (Code a) (Code a)
   | Return (Expr a)

type Expr' = Expr ()
data Expr a where
   Cast      :: Expr a              -> Expr b
   Par       :: Expr a              -> Expr a
   EName     :: Name                -> Expr a -- name
   EAttr     :: Attr                -> Expr a -- expr.name
   Arr       :: Expr a -> Expr a    -> Expr a -- expr[expr]
   Literal   :: Literal             -> Expr a -- lit
   Op        :: OpExpr a            -> Expr a -- expr + expr

   -- untyped
   FuncExpr  :: Code a              -> Expr a -- fuction() { code }
   FuncCall  :: Expr a -> [Expr b]  -> Expr c -- TODO expr(*expr)

   -- typed
   TypedFDef  :: Args a => a -> Code b -> Expr c
   TypedFCall :: (Show a, Args a) => Expr (a, r) -> a -> Expr r

   Ternary   :: Expr a {-JT.Bool-} -> Expr a -> Expr a -> Expr a
   Regex     :: Text -> Text              -> Expr a

   Null      :: Expr a
   Undefined :: Expr a
   True      :: Expr a -- JT.Bool
   False     :: Expr a -- JT.Bool

   Raw       :: Text -> Expr a -- inject raw js code


data OpExpr a where
   OpBinary :: BOp -> Expr a -> Expr a -> OpExpr a
   OpUnary :: UOp -> Expr a -> OpExpr a

data UOp = UMinus | UPlus | TypeOf

data BOp
   = BMinus | BPlus | Mult | Div
   | Eq | NEq | EEq | NEEq
   | And | Or
   | Gt | Lt
   | GEt | LEt

data Literal
   = String Text
   | Double Double
   | Int    Integer
   | Array  [ Expr' ]
   | Object [ (Either Name Expr', Expr') ]

data FormalArgs = FA [Text]

data Name = Name Text

data Attr = Attr Expr' Name


deriving instance Show (Statement a)
deriving instance Show (Expr a)
deriving instance Show (OpExpr a)
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
      Op opExpr -> ev opExpr
     
      FuncExpr code -> "function() { " <> ev code <> ";}"
      FuncCall name exprs -> ev name <> unargs exprs

      TypedFDef as code -> "function"
         <> unargs (args as)
         <> curly (ev code <> ";")
      TypedFCall f as -> par (ev f) <> unargs (args as)

      Ternary b t f -> par (ev b <> "?" <> ev t <> ":" <> ev f)
      Regex pat mod -> sur "/" "/" pat <> mod

      Null -> "null"
      Undefined -> "undefined"
      True -> "true"
      False -> "false"

      -- meta
      Par expr -> par $ ev expr -- parenthesis around
      Raw stm -> stm -- raw js text
      Cast e -> ev e -- change type

      where
         unargs = par . uncomma . map ev


col (k, v) = either ev ev k <> ": " <> ev v


instance E (OpExpr a) where
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
instance ToLit Text        where toLit v = String v
instance ToLit String   where toLit v = String $ T.pack v
instance ToLit TL.Text  where toLit v = String $ TL.toStrict v
instance ToLit a => ToLit [(a, Expr')] where
   toLit li = Object $ map f li
      where f (a,b) = (Right . lit $ a, b)

instance ToLit [ Expr' ] where
   toLit = Array

lit :: ToLit a => a -> Expr'
lit = Literal . toLit

attr :: Expr' -> Name -> Expr'
attr base attname = EAttr $ Attr base attname

instance IsString Name where
   fromString s = Name $ T.pack s
instance IsString Expr' where
   fromString s = EName $ fromString s 

plus a b = Op $ OpBinary BPlus a b

cast :: Expr a -> Expr ()
cast x = Cast x


-- transform (Expr a, (Expr b, .. to 
class Show a => Args a where
   args :: a -> [Expr']
instance Args (Expr a, END) where
   args (e, ()) = [cast e]
instance Args (b, c) => Args (Expr a, (b, c)) where
   args (e, t) = cast e : args t
type END = ()


