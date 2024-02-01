module Template where

import Common.Prelude
import qualified Data.Text as TS
import DOM.JS
import Web

-- | Creates ids, creates variables for the elements, returns a
-- function to bind them.
idsElems :: JS m => CSS m => Int -> m ([Id], [Expr Tag], Expr b)
idsElems n = do
  ids <- replicateM n (cssId $ pure ())
  elems <- mapM (\_ -> let_ Null) ids
  mount <- newf $ do
    forM (zip ids elems) $ \(id, el) -> el .= querySelector id (ex "document")
  return (ids, elems, Cast mount)

data Template a html ctx out = Template
  { templateIds :: [Id]
  , templateMount :: Expr ()
  , templateCreate :: Expr (a -> DocumentFragment)
  , templateUpdate :: Expr (a -> ())
  , templateSsr :: Maybe a -> Html
  , templateGet :: Expr a
  , templateHtml :: html

  , templateOut :: out
  }
makeFields ''Template

class GetTemplate t ctx where
  type Html' t ctx :: Type
  type Html' t ctx = ()

  type In t ctx :: Type
  type In t ctx = ()

  -- | Anything the template needs to pass to outer context.
  type Out t ctx :: Type
  type Out t ctx = ()

  getTemplate :: (Monad m, MonadFix m, JS m) => In t ctx -> m (Template t (Html' t ctx) ctx (Out t ctx))

-- * Helpers

callMounts :: JS m => [Expr a] -> m ()
callMounts li = mapM_ (bare . call0) li

-- | Wrap a list of mounts to a single function
mergeMounts :: JS m => [Expr a] -> m (Expr r)
mergeMounts li = Cast <$> newf (callMounts li)

-- | Create mock create, update, get, html' and ssr functions. Since
-- $template$'s $html$ varies in type then this is returned as plain
-- value.
mock
  :: forall m a x1 x2. JS m
  => TS.Text -> m (Expr (a -> DocumentFragment), Expr x1, Expr x2, Html, Maybe a -> Html)
mock (title :: TS.Text) = do
  let title' = lit title :: Expr String
  create <- newf $ \(_ :: Expr p) -> do
    log $ "mock: create " <> title'
    fragment :: Expr DocumentFragment <- createHtmls $ dyn $ ("mock: create " <> title' :: Expr String)
    -- todo: toHtml is dyn? this works?
    return_ fragment
  update <- newf $ log $ "mock: update " <> title'
  get <- newf $ log $ "mock: get " <> title'
  let htmlMock = div $ "mock: html' " <> toHtml title
      ssr _ = htmlMock
  return (Cast create, Cast update, Cast get, htmlMock, ssr)

mock2
  :: forall m a x1 x2. JS m
  => TS.Text -> m (Expr (a -> DocumentFragment), Expr x1, Expr x2, Html, Maybe a -> Html)
mock2 str = return (Undefined, Undefined, Undefined, toHtml str, \_ -> toHtml str)

-- * Compatibility construcotrs

type Template0 t ctx = Template t (Html' t ctx) ctx (Out t ctx)

mkTemplate0
  :: (Out t ctx ~ ())
  => [Id] -> Expr () -> Expr (t -> DocumentFragment) -> Expr (t -> ())
  -> (Maybe t -> Html) -> Expr t -> (Html' t ctx) -> Template0 t ctx
mkTemplate0 ids mount create update ssr get html =
  Template ids mount create update ssr get html ()

getTemplate0
  :: (GetTemplate t ctx, Monad m, MonadFix m, In t ctx ~ (), JS m)
  => m (Template t (Html' t ctx) ctx (Out t ctx))
getTemplate0 = getTemplate ()
