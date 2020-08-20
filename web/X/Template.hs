module X.Template where

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

data Template a f p = Template
  { templateIds :: [Id]
  , templateMount :: Expr ()
  , templateCreate :: Expr (a -> DocumentFragment)
  , templateUpdate :: Expr (a -> ())
  , templateSsr :: Maybe a -> Html
  , templateGet :: Expr a
  , templateHtml :: f
  }
makeFields ''Template

class GetTemplate a p where
  type Html' a p :: *
  getTemplate :: MonadWeb m => m (Template a (Html' a p) p)

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
  :: forall m a x1 x2. MonadWeb m
  => TS.Text -> m (Expr (a -> DocumentFragment), Expr x1, Expr x2, Html, Maybe a -> Html)
mock (title :: TS.Text) = do
  let title' = lit title :: Expr String
  create <- js $ newf $ \(o :: Expr p) -> do
    log $ "mock: create " <> title'
    fragment :: Expr DocumentFragment <- createHtmls $ toHtml $ ("mock: create " <> title' :: Expr String)
    retrn fragment
  update <- js $ newf $ log $ "mock: update " <> title'
  get <- js $ newf $ log $ "mock: get " <> title'
  let htmlMock = div $ "mock: html' " <> toHtml title
      ssr _ = htmlMock
  return (create, update, get, htmlMock, ssr)
