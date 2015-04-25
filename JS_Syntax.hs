module JS_Syntax 
   ( module JS_Syntax
   , UOp(..), BOp(..)
   )
   where

import Prelude2 hiding (True, False)
import Text.Exts

import Prelude (fromIntegral)
import Data.String
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified JS_Types as JT
import JS_Types (UOp(..), BOp(..))
import Base

type Code a = [ Statement a  ]

-- X=implemented as function, therefore b /= c
data Statement a where
   FuncDef  :: Name -> FormalArgs -> Code a -> Statement b
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
   Arr       :: Expr a -> Expr a    -> Expr a -- expr[expr]
   Literal   :: Literal a           -> Expr a -- lit
   LiteralE2 :: Literal2 a           -> Expr a -- lit
   Op        :: OpExpr  a           -> Expr a -- expr + expr

   -- new
   BOp       :: E (JT.Proxy o) => BOpExpr t o -> Expr t -- expr + expr
   UOp       :: UOpExpr t o         -> Expr t -- expr + expr


   -- untyped
   FuncExpr  :: Code a              -> Expr a -- fuction() { code }
   FuncCall  :: Expr a -> [Expr b]  -> Expr c -- TODO expr(*expr)

   -- typed
   TypedFDef  :: Args a => a -> Code b -> Expr c
   TypedFCall :: (Show a, Args a) => Expr (a, r) -> a -> Expr r

   Ternary   :: Expr JT.Bool -> Expr a -> Expr a -> Expr a
   Regex     :: Text -> Text              -> Expr a

   Null      :: Expr a
   Undefined :: Expr a
   True      :: Expr a -- JT.Bool
   False     :: Expr a -- JT.Bool

   Raw       :: Text -> Expr a -- inject raw js code


data OpExpr a where
   OpBinary :: BOp -> Expr a -> Expr a -> OpExpr a
   OpUnary :: UOp -> Expr a -> OpExpr a

data BOpExpr t o where
   BOE :: JT.O t o => JT.Proxy o -> Expr t -> Expr t -> BOpExpr (JT.D t o) o

data UOpExpr t o where
   UOE :: JT.U t o => Expr t -> UOpExpr t o

instance E (JT.Proxy o) => E (BOpExpr a o) where
   ev (BOE p a b) = ev a <> ev p <> ev b
instance E (JT.Proxy o) => E (UOpExpr a o) where
   ev (UOE a) = ev (JT.Proxy :: JT.Proxy o) <> ev a

{-
-}

data Literal a
   = String Text
   | Double Double
   | Int    Integer
   | Array  [ Expr a ]
   | Object [ (Either Name (Expr a), (Expr a)) ]

data Literal2 a where
   Literal2 :: a -> Literal2 a
data FormalArgs = FA [Text]

data Name = Name Text

data Attr = forall a. Attr (Expr a) Name

{-
deriving instance Show (Statement a)
deriving instance Show (Expr a)
deriving instance Show (OpExpr a)
deriving instance Show UOp
deriving instance Show BOp
deriving instance Show (BOpExpr t o)
deriving instance Show (UOpExpr t o)
deriving instance Show (Literal a)
deriving instance Show FormalArgs
deriving instance Show Name
deriving instance Show Attr
-}



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
      Op opExpr -> ev opExpr
      BOp expr -> ev expr
      -- UOp expr -> ev expr
     
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
instance E (Literal z) where
   ev x = case x of
      String s -> q'' s
      Double s -> tshow s
      Int    s -> tshow s
      Array  li -> ang $ uncomma $ map ev li
      Object li -> curly $ uncomma $ map col li
instance E a => E (Literal2 a) where
   ev (Literal2 a) = ev a




-- * AST creation convenience

class    ToLit a        where toLit :: a -> Literal b
instance ToLit Int      where toLit v = Int $ toInteger v
instance ToLit Integer  where toLit v = Int v
instance ToLit Double   where toLit v = Double v
instance ToLit Text     where toLit v = String v
instance ToLit String   where toLit v = String $ T.pack v
instance ToLit TL.Text  where toLit v = String $ TL.toStrict v
instance ToLit a => ToLit [(a, Expr b)] where
   toLit li = Object $ map f li
      where f (a,b) = (Right . lit $ a, Cast b)
instance ToLit [ Expr a ] where
   toLit = Array . map Cast
lit :: ToLit a => a -> Expr b
lit = Literal . toLit


class JT.HJ a => ToLit2 a where lit2 :: a -> Expr (JT.Dest a)
instance ToLit2 Int      where lit2 = lit2'
instance ToLit2 Integer  where lit2 = lit2'
instance ToLit2 Double   where lit2 = lit2'
instance ToLit2 Text     where lit2 = lit2'
instance ToLit2 String   where lit2 = lit2'
instance ToLit2 TL.Text  where lit2 = lit2'
{-
instance ToLit2 a => ToLit2 [(a, Expr b)] where
   toLit2 li = Object $ map f li
      where f (a,b) = (Right . lit $ a, Cast b)
instance ToLit2 [ Expr a ] where
   toLit2 = Array . map Cast
-}
lit2' = LiteralE2 . Literal2 . JT.jst





attr :: Expr a -> Name -> Expr b
attr base attname = EAttr $ Attr base attname

instance IsString Name where
   fromString s = Name $ T.pack s
instance IsString (Expr a) where
   fromString s = EName $ fromString s 

plus a b = Op $ OpBinary Plus a b

cast :: Expr a -> Expr ()
cast x = Cast x


-- transform (Expr a, (Expr b, .. to 
class Args a      where args :: a   -> [ Expr () ]
instance Args (Expr a, ())  where args (e, ()) = [ Cast e  ]
instance Args (b, c)
   => Args (Expr a, (b, c)) where args (e, t)  = Cast e : args t
