module X.Template.V2 where

import qualified Data.Text as TS
import X.Prelude
import qualified Prelude
import X

-- | Creates ids, creates variables for the elements, returns a
-- function to bind them.
idsElems :: MonadWeb m => Int -> m ([Id], [Expr Tag], Expr b)
idsElems n = do
  ids <- replicateM n (cssId $ pure ())
  js $ do
    elems <- mapM (Prelude.const $ let_ Null) ids
    mount <- newf $ do
      forM (zip ids elems) $ \(id, el) -> el .= findBy id
    return (ids, elems, mount)

-- * Templating

-- ** Context

data Context a
data Node

context :: Expr a -> Expr b -> Expr (Context a)
context a b = lit [a, Cast b]

source :: Expr (Context a) -> Expr a
source ctx = ctx !- 0

nodes :: Expr (Context a) -> Expr [Node]
nodes ctx = ctx !- 1

-- | Iterate through the nodes array, run action
withNodes :: Expr (Context a) -> (Expr Node -> M r b) -> M r ()
withNodes ctx go = iterArray (nodes ctx) $ \ix -> do
  node <- const $ nodes ctx !- ix -- <- remember me
  go node

createContext :: Expr a -> Html -> M r (Expr (Context a))
createContext item html = do
  fragment <- createHtmls html
  nodes <- const $ ex "Array" !// "from" $ fragment !. "childNodes"
  ctx <- const $ context item nodes
  return ctx

-- | Append DOM nodes from @context@ to element with @id@
appendContext :: Expr (Context a) -> Id -> M r ()
appendContext context id = do
  dest <- const $ querySelector id document
  withNodes context $ \node -> do
    bare $ dest !// "appendChild" $ node

-- ** Template

type Create a = Expr a -> Expr (Context a)
type Update a = Expr (Context a) -> Expr ()

data Template a ctx out = Template
  { templateIds :: [Id]
  , templateMount :: Expr ()
  , templateCreate :: Create a
  , templateUpdate :: Update a
  , templateGet :: Expr a

  -- | Both create and ssr map a to the input of html
  , templateSsr :: a -> Html -- ssr

  , templateOut :: out
  }
makeFields ''Template


data SSR a ctx out = SSR
  { sSRIds :: [Id]
  , sSRSsr :: a -> Html
  , sSROut :: out
  }
makeFields ''SSR

data Client a =  Client
  { clientMount :: Expr ()
  , clientCreate :: Create a
  , clientUpdate :: Update a
  , clientGet :: Expr a
  }
makeFields ''Client

class GetTemplate t ctx where
  type Html' t ctx :: *

  type In t ctx :: *
  type In t ctx = ()

  -- | Anything the template needs to pass to outer context.
  type Out t ctx :: *
  type Out t ctx = ()

  getTemplate :: (Monad m, MonadFix m) => In t ctx -> WebT m (Template t ctx (Out t ctx))

-- * Helpers

callMounts :: [Expr a] -> M r ()
callMounts li = mapM_ (bare . call0) li

-- | Wrap a list of mounts to a single function
mergeMounts :: (MonadWeb m) => [Expr a] -> m (Expr r)
mergeMounts li = js $ newf $ callMounts li

-- | Create mock create, update, get, html' and ssr functions. Since
-- $template$'s $html$ varies in type then this is returned as plain
-- value.
mock
  :: forall m a. MonadWeb m
  => TS.Text -> m (Create a, Update a, Expr a, Html, a -> Html)
mock (title :: TS.Text) = do
  let title' = lit title :: Expr String
  create <- js $ fn $ \(a :: Expr a) -> do
    log $ "mock: create " <> title'
    fragment :: Expr DocumentFragment <- createHtmls $ toHtml $ ("mock: create " <> title' :: Expr String)
    retrn $ context a fragment
  update <- js $ fn $ \(_ :: Expr (Context a)) -> do
    log $ "mock: update " <> title'
    retrn (Undefined :: Expr ())
  get <- js $ newf $ log $ "mock: get " <> title'
  let htmlMock = div $ "mock: html' " <> toHtml title
      ssr _ = htmlMock
  return (create, update, get, htmlMock, ssr)

mock2
  :: forall m a. MonadWeb m
  => TS.Text -> m (Expr (a -> DocumentFragment), Expr a, Expr a, Html, Maybe a -> Html)
mock2 str = return (Undefined, Undefined, Undefined, toHtml str, \_ -> toHtml str)

-- * Compatibility construcotrs

type Template0 t ctx = Template t ctx (Out t ctx)

-- | 1. no out
-- mkTemplate0
--   :: (Out t ctx ~ out)
--   => [Id] -> Expr () -> Expr (t -> DocumentFragment) -> Expr (t -> ())
--   -> Expr t -> (t -> Html) -> out -> Template0 t ctx
mkTemplate0 ids mount create update get ssr out =
  Template ids mount create update get ssr out

-- | 2. only ssr
ssrOnly :: (t -> Html) -> Template0 t ctx
ssrOnly ssr = Template todo todo todo todo todo ssr todo

emptyTemplate :: forall k a (ctx :: k) out. Template a ctx out
emptyTemplate = Template
   [] Undefined (Prelude.const Undefined) (Prelude.const Undefined) Undefined (Prelude.const "") undefined

getTemplate0
  :: (GetTemplate t ctx, Monad m, MonadFix m, In t ctx ~ ())
  => WebT m (Template t ctx (Out t ctx))
getTemplate0 = getTemplate ()
