module JS.Render
  ( module JS.Render
  , JT.Conf(..)
  ) where

import Prelude2 hiding (True, False, Empty, intercalate, unwords, unlines, replicate, Const)
import Control.Monad.Reader

import Render
import JS.Syntax hiding ((=:))
import qualified JS.Types as JT

-- * Print AST to JavaScript

instance Render (Code a) where
  type Conf (Code a) = JT.Conf
  renderM li = do
    conf <- ask
    case conf of
      JT.Indent n -> uncode li <&> unlines
      JT.Minify -> uncode li <&> mconcat


instance Render (Statement a) where
  type Conf (Statement a) = JT.Conf
  renderM stm = (<+> pure ";") $ case stm of
    Var name -> var name
    VarDef name exp -> var name =: renderM exp
    BareExpr expr -> renderM expr
    For init cond post conts -> mseq
      [ pure "for"
      , par . unsemi <$> sequence [renderM init, renderM cond, renderM post]
      , curlyCode conts
      ]
    ForIn name expr code -> mseq
      [ pure "for"
      , par <$> mseq
          [ var name
          , pure " in "
          , renderM expr
          ]
      , curlyCode code
      ]
    While cond code -> mseq
      [ pure "while"
      , par <$> renderM cond
      , curlyCode code
      ]
    IfElse cond true mbElse -> mseq
      [ pure "if"
      , par <$> renderM cond
      , curlyCode true
      , maybe (pure "") (\else_ -> ("else" <>) <$> curlyCode else_) mbElse
      ]
    Return expr -> pure "return " <+> renderM expr
    Empty -> pure ""
    Let name exp -> define "let " name =: renderM exp
    Const name exp -> define "const " name =: renderM exp
    where
      define kw name = pure kw <+> renderM name
      var = define "var "

instance Render (Expr a) where
  type Conf (Expr a) = JT.Conf
  renderM expr = case expr of
    Assign e1 e2 -> renderM e1 =: renderM e2
    EName name -> renderM name
    Arr arrExpr ixExpr -> renderM arrExpr <+> (ang <$> renderM ixExpr)
    In str obj -> renderM str <+> pure " in " <+> renderM obj
    EAttr attr -> renderM attr
    ULit lit -> renderM lit
    Op opExpr -> renderM opExpr
    FuncCall name exprs -> renderM name <+> unargs exprs
    AnonFunc mbName as code -> function "function " mbName as code
    Generator mbName as code -> function "function* " mbName as code
    Async mbName as code -> function "async function " mbName as code
    TypedFCall f as -> (par <$> renderM f) <+> (unargs $ args as)
    -- DELME TypedFDef as code -> "function" <> unargs (args as) <> uncode code
    Ternary b t f -> par <$> mseq [renderM b, pure "?", renderM t, pure ":", renderM f]
    Null -> pure "null"
    Undefined -> pure "undefined"
    -- meta
    Par expr -> par <$> renderM expr -- parenthesis around
    Raw stm -> pure stm -- raw js text
    Cast e -> renderM e -- change type

    Yield e -> pure "yield " <+> renderM e
    YieldDelegate e -> pure "yield* " <+> renderM e
    Await e -> pure "await " <+> renderM e

    where
      function kw mbName as code = mseq
        [ pure $ kw <> maybe "" getName mbName
        , unargs as
        , pure " "
        , curlyCode code
        ]

a =: b = a <+> pure eq' <+> b
eq' = " = "
unargs li = par . uncomma <$> mapM renderM li

instance Render (OpExpr a) where
  type Conf (OpExpr a) = JT.Conf
  renderM o = case o of
    OpBinary op e1 e2 -> par <$> mseq [renderM e1, renderM op, renderM e2]
    OpUnary op e -> error "web:JS.Syntax.hs:Render OpUnary not implemented"

instance Render UOp where
  type Conf UOp = JT.Conf
  renderM op = pure $ case op of
    UMinus -> "-"; UPlus -> "+"
    TypeOf -> "typeof "
    Not -> "!"
    -- Increment -> "++"; PrefixDecrement -> "--" -- post

instance Render BOp where
  type Conf BOp = JT.Conf
  renderM op = pure $ case op of
    Minus -> "-"  ; Plus -> "+"
    Mult  -> "*"  ; Div  -> "/"
    Modulus -> "%"
    Eq    -> "==" ; NEq  -> "!="
    EEq   -> "==="; NEEq -> "!=="
    And   -> "&&" ; Or   -> "||"
    Lt    -> "<"  ; Gt   -> ">"
    LEt   -> "<=" ; GEt  -> ">="

instance Render Attr where
  type Conf Attr = JT.Conf
  renderM (Attr exp name) = sur "(" ")" <$> (inf "." <$> renderM exp <*> renderM name)

instance Render Name where
  type Conf Name = JT.Conf
  renderM (Name s) = pure s

-- ** Literals

instance Render ULiteral where
  type Conf ULiteral = JT.Conf
  renderM ul = case ul of
    ULString text -> pure $ q'' text
    ULDouble dbl -> pure $ tshow dbl
    ULInteger i -> pure $ tshow i
    ULBool b -> pure $ toLower $ tshow b
    ULArray li -> ang . uncomma <$> mapM renderM li
    ULObject obj -> curly . uncomma <$> mapM f obj
      where f (e,expr) = either renderM renderM e <+> pure ":" <+> renderM expr

-- * Helpers

-- | Put printed code in curly braces, code in braces is indented.
curlyCode :: Code a -> Reader (Conf (Code a)) Text
curlyCode code = do
  conf <- ask
  inner <- case conf of
    JT.Indent n -> do
      stms :: [Text] <- withReader (inc 2) $ uncode code
      return $ "\n" <> mconcat (map (<> "\n") stms) <> spaces n
      where
        inc m (JT.Indent n) = JT.Indent (m + n)
        inc _ c = c
    JT.Minify -> uncode code <&> mconcat
  return $ "{" <> inner <> "}"

uncode
 :: (Render (Code a), Conf (Code a) ~ JT.Conf)
 => Code a -> Reader (Conf (Code a)) [Text]
uncode code = do
  conf <- ask
  case conf of
    JT.Indent n -> mapM renderM code <&> map (spaces n <>)
    JT.Minify -> mapM renderM code

-- * Simple helpers

spaces n = replicate (fromIntegral n) " "

inf i a b = a <> i <> b
col (k, v) = inf ": " <$> either renderM renderM k <*> renderM v
