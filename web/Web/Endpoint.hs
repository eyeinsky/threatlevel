module Web.Endpoint
  ( module Web.Endpoint
  , module Web.Response
  ) where


import Pr hiding (Reader, Writer, State, (/))

import Control.Monad.State (get, put)
import HTTP.Common (ToPayload(..))

import qualified URL
import URL (URL, Segment)
import qualified Web as W
import Web (Browser)
import qualified HTTP.Header as Hdr
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text as TS
import Render
import Identifiers (identifierSource)

import Web.Browser (browser)
import qualified JS.DSL
import qualified JS
import DOM hiding (M)

import qualified Web.Response as Re
import Web.Response (renderURL)
import qualified Network.Wai as Wai
import qualified Trie as Tr

import Text.Boomerang.Texts
import Text.Boomerang.TH
import Text.Boomerang hiding ((.~))
import qualified Text.Boomerang.Texts as B

import qualified HTTP.Response as HR


type M m r = W.WebT (ReaderT r m)

runM :: W.Conf -> r -> W.State -> M m r a -> m (a, W.State, W.Writer)
runM mc r st m = m
  & W.runWebMT mc st
  & flip runReaderT r

type State = [Segment]
type Writer r = [(Segment, T r)]

(/) :: MonadWriter [(Segment, T r)] m => Segment -> T r -> m ()
segment / endpoint = tell [(segment, endpoint)]

type InT r = RWST URL (Writer r) State (M IO r) (EHandler r)
data T r where T :: InT r -> T r

runT :: URL -> T r -> M IO r (EHandler r, State, Writer r)
runT url (T m) = runRWST m url identifierSource

-- * Build

type EHandler r = Wai.Request -> M IO r Re.Response
type HandlePoint r = (URL, (EHandler r, W.State, W.Writer))
type Built r = Tr.Trie Segment (EHandler r, W.State, W.Writer)

build
  :: (W.HasBrowser r W.Browser)
  => W.Conf -> W.State -> URL -> r -> T r -> IO (Built r)
build mc ms domain r m = Tr.fromList <$> list
  where
    list = eval mc ms domain r m <&> fmap (_1 %~ tail . Re.toTextList)

    eval :: (W.HasBrowser r W.Browser)
      => W.Conf -> W.State -> URL -> r -> T r -> IO [HandlePoint r]
    eval mc js_css_st0 url (r :: r) (m :: T r) = do
      ((main, _ {-stUrls-}, subs), js_css_st1, js_css_w) <- runM mc r js_css_st0 (runT url m)
      let
        self = (url, (main, js_css_st1, js_css_w)) :: HandlePoint r

        re :: (Segment, T r) -> IO [HandlePoint r]
        re (segm, sub) = eval mc js_css_st1 (url & URL.segments <>~ [segm]) r sub

      result :: [HandlePoint r] <- mapM re subs <&> mconcat ^ (self :)
      return result

-- * Run

handle
  :: W.Conf
  -> r
  -> Wai.Request
  -> (EHandler r, W.State, W.Writer)
  -> IO Re.Response
handle mc r req (i_io, js_css_st, js_css_wr) = merge <$> res
  where
    res = runM mc r js_css_st (i_io req)
    merge (Re.Response s h (Re.HtmlDocument doc), st, wr) = Re.Response s h $ Re.HtmlDocument $ collapse (js_css_wr <> wr) doc
    merge (other, _, _) = other

    collapse :: W.Writer -> W.Document -> W.Document
    collapse code doc
      = doc
      & add (W.style $ W.raw $ render () $ code ^. W.cssCode)
      & add (W.script $ W.raw $ render (mc^.W.jsConf.JS.renderConf) $ code ^. W.jsCode)
      where add w = W.head' %~ (>> w)

-- * To handler

class HasDynPath s a | s -> a where
  dynPath :: Lens' s a
  {-# MINIMAL dynPath #-}

type Confy r = (HasDynPath r [URL.Segment], W.HasBrowser r W.Browser)

toHandler
  :: forall r. Confy r
  => W.Conf -> W.State -> URL -> r -> T r
  -> IO (Wai.Request -> IO (Maybe Re.Response))
toHandler mc ms domain conf0 site = do
  app <- build mc ms domain conf0 site
  return $ \req -> let
    hdrs = Wai.requestHeaders req
    path = Wai.pathInfo req :: [Segment]
    browser' = maybe W.Unknown W.parseBrowser $ lookup "User-Agent" hdrs
    conf1 = conf0 & W.browser .~ browser'
    res :: Maybe (EHandler r, W.State, W.Writer)
    (res, conf2) = case Tr.lookupPrefix path app of
      Just (pathSuffix, maybeValue, _) -> (maybeValue, conf1 & dynPath .~ pathSuffix)
      _ -> (Nothing, conf1)
    in traverse (handle mc conf2 req) res

-- * Dyn path

parseDyn
  :: (MonadReader s f, HasDynPath s [Segment])
  => Boomerang TextsError [Segment] () (r :- ())
  -> f (Either TextsError r)
parseDyn parser = asks (view dynPath) <&> parseTexts parser

renderDyn :: Boomerang e [Segment] () (r :- ()) -> r -> URL -> URL
renderDyn pp dt url = url & URL.segments <>~ fromJust (unparseTexts pp dt)

-- * API

currentUrl :: MonadReader URL m => m URL
currentUrl = ask

api
  :: (MonadState [Segment] m, MonadReader URL m, MonadWriter [(Segment, T r)] m)
  => InT r -> m URL
api m = next >>= flip pin m

xhrPost' m = do
  url :: URL <- api m
  lift . W.js . fmap JS.Par . JS.func JS.AnonFunc $ \data_ -> xhrPost (JS.lit $ renderURL $ url) data_ []

-- | Add segment with api endpoint and return its full url
pin
  :: (MonadWriter [(Segment, T r)] m, MonadReader URL m)
  => Segment -> InT r -> m URL
pin name m = name / T m *> nextFullWith name

page = api . return . (\response _ -> return response) . Re.page

next :: MonadState [Segment] m => m Segment
next = get >>= \(x : xs) -> put xs *> return x

nextFullWith :: MonadReader URL m => Segment -> m URL
nextFullWith top = ask <&> URL.segments <>~ [top]

-- * Helpers

staticResponse :: (Monad m1, Monad m2) => a -> m1 (p -> m2 a)
staticResponse response = return $ \req -> return response

-- | Take url, a unparser, value, and unparse the value to the end of url
mkUrl :: URL.URL -> Boomerang e [TS.Text] () (r :- ()) -> r -> URL.URL
mkUrl url unparser value = url & URL.segments <>~ segm
  where segm = B.unparseTexts unparser value & fromJust
