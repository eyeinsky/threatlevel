{-# LANGUAGE RecordWildCards #-}
module Template.V2
  ( module Template.V2
  , module Template.Common
  ) where

import Common.Prelude
import qualified Prelude
import qualified Data.Text as TS
import JS
import HTML
import CSS hiding (Tag)
import Web.Apis.DOM
import X.Template.Common

-- | Creates ids, creates variables for the elements, returns a
-- function to bind them.
idsElems :: JS m => Int -> m ([Id], [Expr Tag], Expr b)
idsElems n = do
  ids <- replicateM n (cssId $ pure ())
  elems <- mapM (Prelude.const $ let_ Null) ids
  mount <- newf $ do
    forM (zip ids elems) $ \(id, el) -> el .= querySelector id document
  return (ids, elems, mount)

-- * Template

type Create a = Expr a -> Expr (Context a)
type Update a = Expr a -> Expr ()

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
  type In t ctx :: *
  type In t ctx = ()

  -- | Anything the template needs to pass to outer context.
  type Out t ctx :: *
  type Out t ctx = ()

  getTemplate :: (JS m, Monad m, MonadFix m) => In t ctx -> m (Template t ctx (Out t ctx))

-- * Helpers

callMounts :: JS m => [Expr a] -> m ()
callMounts li = mapM_ (bare . call0) li

-- | Wrap a list of mounts to a single function
mergeMounts :: (JS m) => [Expr a] -> m (Expr r)
mergeMounts li = newf $ callMounts li

-- | Create mock create, update, get, html' and ssr functions. Since
-- $template$'s $html$ varies in type then this is returned as plain
-- value.
mock
  :: forall m a. JS m
  => TS.Text -> m (Create a, Update a, Expr a, Html, a -> Html)
mock (title :: TS.Text) = do
  let title' = lit title :: Expr String
  create <- fn $ \(a :: Expr a) -> do
    log $ "mock: create " <> title'
    fragment :: Expr DocumentFragment <- createHtmls $ toHtml $ ("mock: create " <> title' :: Expr String)
    return_ $ context a (fragment2nodes fragment) fragment
  update :: Update a <- fn $ \(_ :: Expr a) -> do
    log $ "mock: update " <> title'
    return_ (Undefined :: Expr ())
  get <- newf $ log $ "mock: get " <> title'
  let htmlMock = div $ "mock: html' " <> toHtml title
      ssr _ = htmlMock
  return (create, update, get, htmlMock, ssr)

mock2
  :: forall m a. JS m
  => TS.Text -> m (Expr (a -> DocumentFragment), Expr a, Expr a, Html, Maybe a -> Html)
mock2 str = return (Undefined, Undefined, Undefined, toHtml str, \_ -> toHtml str)

nTemplate
  :: forall m t ctx. JS m
  => Int -> m (Template t ctx ())
nTemplate n = do
  (templateIds, _, templateMount) <- idsElems n
  templateCreate <- fn $ \o -> do
    throw "templateUpdate not implemented"
    ctx <- createContext o $ "temlpateCreate not implemented"
    return_ ctx
  templateUpdate <- fn $ \(_ :: Expr t) -> do
    throw "templateUpdate not implemented"
    return_ (Undefined :: Expr ())
  templateGet <- newf $ do
    throw "templateGet not implemented"
    return_ (Undefined :: Expr t)
  let
    templateSsr _ = error "SSR not implemented"
    templateOut = ()
  return $ Template {..}

-- * Compatibility construcotrs

type Template_ t ctx = Template t ctx ()

-- | 1. no out
-- mkTemplate0
--   :: (Out t ctx ~ out)
--   => [Id] -> Expr () -> Expr (t -> DocumentFragment) -> Expr (t -> ())
--   -> Expr t -> (t -> Html) -> out -> Template0 t ctx
mkTemplate_ ids mount create update get ssr =
  Template ids mount create update get ssr ()

mkTemplate0 = mkTemplate_
{-# DEPRECATED mkTemplate0 "Use `mkTemplate_` instead." #-}

-- | 2. only ssr
ssrOnly :: (t -> Html) -> Template_ t ctx
ssrOnly ssr = Template todo todo todo todo todo ssr todo

emptyTemplate :: forall k a (ctx :: k). Template a ctx ()
emptyTemplate = Template
   [] Undefined (\_ -> Undefined) (\_ -> Undefined) Undefined (\_ -> "") ()

getTemplate_, getTemplate0
  :: (GetTemplate t ctx, Monad m, MonadFix m, In t ctx ~ (), JS m)
  => m (Template t ctx (Out t ctx))
getTemplate_ = getTemplate ()
{-# DEPRECATED getTemplate0 "Use `getTemplate_` instead." #-}

getTemplate0 = getTemplate_
