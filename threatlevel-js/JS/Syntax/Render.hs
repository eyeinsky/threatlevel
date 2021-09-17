{-# OPTIONS_GHC -Wno-orphans #-}
module JS.Syntax.Render (Conf(..), unargs) where

import Prelude
import Data.Default
import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Functor

import Render hiding (Conf)
import qualified Render
import JS.Syntax.Types

-- * Print AST to JavaScript

data Conf
  = Indent Int
  | Minify
  deriving (Eq, Show, Read)

instance Default Conf where
  def = Indent 2

type RenderJS a = (Render a, Render.Conf a ~ Conf)

instance Render (Code a) where
  type Conf (Code a) = Conf
  renderM li = do
    conf <- ask
    case conf of
      Indent _ -> uncode li <&> TL.unlines
      Minify -> uncode li <&> mconcat


instance Render (Statement a) where
  type Conf (Statement a) = Conf
  renderM stm = (<+> pure ";") $ case stm of
    Var name -> var name
    VarDef name exp -> var name =: renderM exp
    BareExpr expr -> renderM expr
    For init cond post conts -> mseq
      [ pure "for"
      , par . unsemi <$> sequence [renderM init, renderM cond, renderM post]
      , curlyCode conts
      ]
    ForIn name expr code -> mseq $ loop name expr code var " in "
    ForOf name expr code -> mseq $ loop name expr code const " of "

    ForAwait name expr code -> mseq
      [ pure "for await"
      , par <$> mseq
          [ const name
          , pure " of "
          , renderM expr
          ]
      , curlyCode code
      ]

    While cond code -> mseq
      [ pure "while"
      , par <$> renderM cond
      , curlyCode code
      ]

    Continue maybeLabel -> withLabel "continue" maybeLabel
    Break maybeLabel -> withLabel "break" maybeLabel

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
    FuncDefStm name as code -> function "function " (Just name) as code
    TryCatchFinally try catches maybeFinally ->
      pure "try" <+> curlyCode try
      <+> (mconcat <$> mapM mkCatchBlock catches)
      <+> maybe (pure mempty)
          (fmap ("finally" <>) . curlyCode) maybeFinally
      where
        mkCatchBlock
          :: (Name, Code r) -> ReaderT Conf Identity TL.Text
        mkCatchBlock (err, code) = mseq
          [ pure "catch"
          , par <$> renderM err
          , curlyCode code
          ]

    Throw e -> pure "throw " <+> renderM e

    Switch e cases def -> let
        mkCase (match, code) = let
          str = indented code :: Reader Conf TL.Text
          in pure "case " <+> renderM match <+> pure ":" <+> str

        pre = pure "switch" <+> (par <$> renderM e)

        body :: Reader Conf Text
        body = withReader (inc 2) $ let
          cases' = map mkCase cases :: [Reader Conf Text]
          def' = maybe (pure "") (\def -> pure "default:" <+> indented def) def
          in mseq $ nl : cases' <> [def']
      in pre <+> (curly <$> body)

    Class name maybeExtends bodyParts -> mseq
      [ pure "class "
      , renderM name
      , maybe (pure "") (\name -> pure " extends " <+> renderM name) maybeExtends
      , pure " "
      , Render.curly <$> indented bodyParts
      ]

    where
      define kw name = pure kw <+> renderM name
      var = define "var "
      const = define "const "
      withLabel statement maybeLabel = case maybeLabel of
        Just label -> pure statement <+> pure " " <+> renderM label
        _ -> pure statement
      loop name expr code varDecl loopType =
        [ pure "for"
        , par <$> mseq
          [ varDecl name
          , pure loopType
          , renderM expr
          ]
        , curlyCode code
        ]

instance Render (Expr a) where
  type Conf (Expr a) = Conf
  renderM expr = case expr of
    Assign e1 e2 -> renderM e1 =: renderM e2
    EName name -> renderM name
    Arr arrExpr ixExpr -> renderM arrExpr <+> (ang <$> renderM ixExpr)
    In str obj -> renderM str <+> pure " in " <+> renderM obj
    EAttr attr -> renderM attr
    Lit lit -> renderM lit
    Op opExpr -> renderM opExpr
    FuncCall name exprs -> renderM name <+> unargs exprs
    AnonFunc mbName as code -> function "function " mbName as code
    Generator mbName as code -> function "function* " mbName as code
    Async mbName as code -> function "async function " mbName as code
    TypedFCall f as -> (par <$> renderM f) <+> (unargs $ args as)
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

    New e -> pure "new " <+> renderM e

    -- TypedFDef _ _ -> todo

instance Render ClassBodyPart where
  type Conf ClassBodyPart = Conf
  renderM = \case
    ClassBodyMethod methodType body ->
      renderM methodType <+> pure " " <+> curlyCode body
    ClassBodyField fieldType value ->
      renderM fieldType <+> pure " " <+> renderM value

instance Render ClassBodyMethodType where
  type Conf ClassBodyMethodType = Conf
  renderM mt = case mt of
    Constructor args -> pure "constructor" <+> unargs args
    InstanceMethod name args -> renderM name <+> unargs args
    StaticMethod name args -> prefix "static" name <+> unargs args
    Getter name -> prefix "get" name <+> pure "()"
    StaticGetter name -> prefix "static get" name <+> pure "()"
    Setter name arg -> prefix "set" name <+> (par <$> renderM arg)

instance Render ClassBodyFieldType where
  type Conf ClassBodyFieldType = Conf
  renderM = \case
    Instance name -> renderM name
    Static name -> prefix "static" name
    Private name -> pure "#" <+> renderM name

prefix :: Text -> Name -> ReaderT (Render.Conf Name) Identity Text
prefix word a = pure word <+> pure " " <+> renderM a

function :: TL.Text -> Maybe Name -> [Name] -> Code b -> ReaderT Conf Identity TL.Text
function kw mbName as code = mseq
  [ pure $ kw <> TL.fromStrict (maybe "" getName mbName)
  , unargs as
  , pure " "
  , curlyCode code
  ]

a =: b = a <+> pure eq' <+> b
eq' = " = "
unargs li = par . uncomma <$> mapM renderM li

instance Render (OpExpr a) where
  type Conf (OpExpr a) = Conf
  renderM o = case o of
    OpBinary op e1 e2 -> par <$> mseq [renderM e1, renderM op, renderM e2]
    OpUnary op e -> par <$> renderM op <+> pure " " <+> renderM e

instance Render UOp where
  type Conf UOp = Conf
  renderM op = pure $ case op of
    UMinus -> "-"; UPlus -> "+"
    TypeOf -> "typeof "
    Not -> "!"
    -- Increment -> "++"; PrefixDecrement -> "--" -- post

instance Render BOp where
  type Conf BOp = Conf
  renderM op = pure $ case op of
    Minus -> "-"  ; Plus -> "+"
    Mult  -> "*"  ; Div  -> "/"
    Modulus -> "%"
    Eq    -> "==" ; NEq  -> "!="
    EEq   -> "==="; NEEq -> "!=="
    And   -> "&&" ; Or   -> "||"
    Lt    -> "<"  ; Gt   -> ">"
    LEt   -> "<=" ; GEt  -> ">="

    Instanceof -> " instanceof "

instance Render Attr where
  type Conf Attr = Conf
  renderM (Attr exp name) = inf "." <$> renderM exp <*> renderM name

instance Render Name where
  type Conf Name = Conf
  renderM (Name s) = pure $ TL.fromStrict s

-- ** Literals

instance Render Literal where
  type Conf Literal = Conf
  renderM ul = case ul of
    String text -> pure $ sur . esc . nl . cr $ TL.fromStrict text
      where
        sur str = "\"" <> str <> "\""
        esc = TL.replace "\"" "\\\""
        nl = TL.replace "\n" "\\n"
        cr = TL.replace "\r" "\\r"
    RegExp text opts -> let
      pat = TL.replace "/" "\\/" (TL.fromStrict text)
      in pure $ "/" <> pat <> "/" <> TL.fromStrict opts
    Double dbl -> pure $ tshow dbl
    Integer i -> pure $ tshow i
    Bool b -> pure $ TL.toLower $ tshow b
    Array li -> ang . uncomma <$> mapM renderM li
    Object obj -> curly . uncomma <$> mapM f obj
      where
        f (e,expr) = either renderM cpn e <+> pure ":" <+> renderM expr
        -- computed property name:
        cpn e = pure "[" <+> renderM e <+> pure "]"

-- * Helpers

-- | Put printed code in curly braces, code in braces is indented.
curlyCode :: RenderJS a => [a] -> Reader (Render.Conf a) TL.Text
curlyCode code = Render.curly <$> indented code

-- | Render & indent code
indented :: RenderJS a => [a] -> Reader Conf TL.Text
indented code = ask >>= \case
  Indent n -> do
    stms :: [Text] <- withReader (inc 2) $ uncode code
    return $ "\n" <> mconcat (map (<> "\n") stms) <> spaces n
    where
  Minify -> uncode code <&> mconcat

uncode :: RenderJS a => [a] -> Reader (Render.Conf a) [Text]
uncode code = ask >>= \case
  Indent n -> mapM renderM code <&> map (spaces n <>)
  Minify -> mapM renderM code

-- | Newline
nl :: Reader Conf Text
nl = ask >>= \conf -> pure $ case conf of
  Indent n -> "\n" <> spaces n
  _ -> ""

inc :: Int -> Conf -> Conf
inc m conf = case conf of
  Indent n -> Indent (m + n)
  _ -> conf

-- ** Non-monadic

spaces :: Int -> TL.Text
spaces n = TL.replicate (fromIntegral n) " "

inf :: Text -> Text -> Text -> Text
inf i a b = a <> i <> b
