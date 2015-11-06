module JS_Syntax
   ( module JS_Syntax
   , module Common
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

import Control.Monad.Reader

import qualified JS_Types as JT
import JS_Types (UOp(..), BOp(..))
import Common

type Code a = [Statement a]

data Statement a where
   FuncDefStm :: Name -> FormalArgs -> Code a -> Statement b
   Var      :: Name -> Statement a
   VarDef   :: Name -> [Name] -> Expr a -> Statement b -- a = [b = c =] expr
   AttrDef  :: Attr -> Expr a -> Statement b -- TODO multiple attrs
   Def      :: Expr a -> Expr b -> Statement c
   BareExpr :: Expr a -> Statement b
   IfElse   :: Expr b -> Code r -> Maybe (Code r) -> Statement r
   For      :: Statement a -> Expr b -> Statement c -> Code d{-X-} -> Statement e -- NOTE: X = implemented as function, therefore b /= c
            -- init           cond      post           body
   ForIn    :: Name -> Expr a -> Code b{-X-} -> Statement c

   TryCatch :: Code r -> Code r -> Statement r
   Return   :: Expr a -> Statement a

data Expr a where
   Cast      :: Expr a              -> Expr b
   Raw       :: Text                -> Expr a -- inject raw js code
   Par       :: Expr a              -> Expr a -- parenthesis
   EName     :: Name                -> Expr a -- name
   EAttr     :: Attr                -> Expr a -- expr.name
   Arr       :: Expr a -> Expr b    -> Expr c -- expr[expr]
   -- untyped
   ULit      :: ULiteral            -> Expr a -- 1
   Op        :: OpExpr  a           -> Expr a -- expr + expr
   -- typed
   Literal   :: Literal a => a             -> Expr a -- 1
   BOp       :: E (Proxy o) => BOpExpr t o -> Expr t -- expr + expr
   UOp       :: UOpExpr t o                -> Expr t -- expr + expr
   -- untyped
   FuncCall  :: Expr a -> [Expr b]  -> Expr c -- func(*expr)
   -- typed
   FuncDef    :: [Expr a] -> Code b    -> Expr c
   TypedFDef  :: Args a => a -> Code b -> Expr c
   TypedFCall :: (Show a, Args a) => Expr (a, r) -> a -> Expr r

   Ternary   :: Expr JT.Bool -> Expr a -> Expr a -> Expr a
   Null      :: Expr a
   Undefined :: Expr a

data FormalArgs = FA [Text]
data Name = Name Text
data Attr = forall a. Attr (Expr a) Name

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
   ev (BOE p a b) = mseq [ev a, ev p, ev b]
instance E (Proxy o) => E (UOpExpr a o) where
   ev (UOE a) = mseq [ev (Proxy :: Proxy o), ev a]

-- * Print AST to JavaScript

instance E (Code a) where
   ev li = T.unlines <$> uncode li

instance E (Statement a) where
   ev stm = case stm of
      AttrDef attr exp -> ev attr =: ev exp
      Var name -> pure "var " <+> ev name
      VarDef n ns exp -> (pure "var " <+> ev n) =: ((T.intercalate eq <$> mapM ev ns) <+> ev exp)
      Def e1 e2 -> ev e1 =: ev e2
      BareExpr expr -> ev expr
      For init cond post conts -> mseq
         [ pure "for"
         , par . unsemi <$> sequence [ev init, ev cond, ev post]
         , curlyCode conts
         ]
      ForIn name expr code -> mseq
         [ pure "for"
         , par <$> mseq
             [ ev (Var name)
             , pure " in "
             , ev expr
             ]
         , curlyCode code
         ]
      IfElse cond true mbElse -> mseq
         [ pure "if"
         , par <$> ev cond
         , curlyCode true
         , maybe (pure "") (\else_ -> ("else" <>) <$> curlyCode else_) mbElse
         ]
      Return expr -> pure "return " <+> ev expr
      where
         a =: b = a <+> pure eq <+> b
         eq = " = "

instance E (Expr a) where
   ev expr = case expr of
      EName name -> ev name
      Arr arrExpr ixExpr -> ev arrExpr <+> (ang <$> ev ixExpr)
      EAttr attr -> ev attr
      Literal lit -> ev lit
      ULit lit -> ev lit
      Op opExpr -> ev opExpr
      BOp expr -> ev expr
      -- UOp expr -> ev expr
      FuncCall name exprs -> ev name <+> unargs exprs
      FuncDef as code -> mseq
         [ pure "function"
         , unargs as
         , pure " "
         , curlyCode code
         ]
      TypedFCall f as -> (par <$> ev f) <+> (unargs $ args as)
      -- DELME TypedFDef as code -> "function" <> unargs (args as) <> uncode code
      Ternary b t f -> par <$> mseq [ev b, pure "?", ev t, pure ":", ev f]
      Null -> pure "null"
      Undefined -> pure "undefined"
      -- meta
      Par expr -> par <$> ev expr -- parenthesis around
      Raw stm -> pure stm -- raw js text
      Cast e -> ev e -- change type

unargs li = par . uncomma <$> mapM ev li

instance E (OpExpr a) where
   ev o = case o of
      OpBinary op e1 e2 -> mseq [ev e1, ev op, ev e2]
      OpUnary op e -> error "web:JS_Syntax.hs:E OpUnary not implemented"

instance E BOp where
   ev op = pure $ case op of
      Minus -> "-"  ; Plus -> "+"
      Mult  -> "*"  ; Div  -> "/"
      Eq    -> "==" ; NEq  -> "!="
      EEq   -> "==="; NEEq -> "!=="
      And   -> "&&" ; Or   -> "||"
      Lt    -> "<"  ; Gt   -> ">"
      LEt   -> "<=" ; GEt  -> ">="

instance E Attr where
   ev (Attr exp name) = sur "(" ")" <$> (inf "." <$> ev exp <*> ev name)

instance E Name where
   ev (Name s) = pure s

-- ** Literals

-- *** Typed Literals

instance E JT.Bool   where ev (JT.Bool a) = pure $ T.toLower $ tshow a
instance E JT.Number where ev (JT.Number a) = pure $ tshow a
instance E JT.NumberI where ev (JT.NumberI a) = pure $ tshow a
instance E JT.String where ev (JT.String a) = pure $ q'' a
instance E JT.Object where ev (JT.Object) = pure $ u
instance E JT.Regex where ev (JT.Regex pat mod) = pure $ sur "/" "/" pat <> mod

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
      ULString text -> pure $ q'' text
      ULDouble dbl -> pure $ tshow dbl
      ULInteger i -> pure $ tshow i
      ULBool b -> pure $ T.toLower $ tshow b
      ULArray li -> ang . uncomma <$> mapM ev li
      ULObject obj -> curly . uncomma <$> mapM f obj
         where f (e,expr) = either ev ev e <+> pure ":" <+> ev expr

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

-- * Helpers

-- | Put printed code in curly braces, code in braces is indented.
curlyCode :: Code a -> ER T.Text
curlyCode code = do
   stms :: [T.Text] <- indentCode code
   indent <- flip T.replicate " " <$> ask
   return $ "{\n"
         <> T.intercalate "\n" stms
         <> "\n" <> indent <> "}"
   where
      indentCode = withReader (+2) . uncode

uncode :: Code a -> ER [T.Text]
uncode code = do
   indent <- flip T.replicate " " <$> ask
   stms :: [T.Text] <- mapM ev code
   return $ map (fmtStm indent) stms

fmtStm indent stm = indent <> stm <> ";"

inf i a b = a <> i <> b
a <+> b = (<>) <$> a <*> b
col (k, v) = inf ": " <$> either ev ev k <*> ev v
