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


type M m r = W.WebT (ReaderT r m)

runM :: W.Conf -> r -> W.State -> M m r a -> m (a, W.State, W.Writer)
runM mc r st m = m
  & W.runWebMT mc st
  & flip runReaderT r

type State = [Segment]
type Writer r = [(Segment, T r)]

(/) :: MonadWriter [(Segment, T r)] m => Segment -> T r -> m ()
segment / endpoint = tell [(segment, endpoint)]

data T r where
  T :: RWST URL (Writer r) State (M Identity r) (EHandler r) -> T r
runT url (T m) = runRWST m url identifierSource

-- * Build

type EHandler r = Wai.Request -> M IO r Re.AnyResponse
type HandlePoint r = (URL, (EHandler r, W.State, W.Writer))

eval :: (W.HasBrowser r W.Browser)
  => W.Conf -> W.State -> URL -> r -> T r -> [HandlePoint r]
eval mc js_css_st0 url (r :: r) (m :: T r) = let
    ((main, _ {-stUrls-}, subs), js_css_st1, js_css_w)
      = runIdentity (runM mc r js_css_st0 (runT url m))

    self = (url, (main, js_css_st1, js_css_w)) :: HandlePoint r

    re :: (Segment, T r) -> [HandlePoint r]
    re (segm, sub) = eval mc js_css_st1 (url & URL.segments <>~ [segm]) r sub

  in self : (re =<< subs) :: [HandlePoint r]

build :: (W.HasBrowser r W.Browser)
  => W.Conf -> URL -> r -> T r -> Tr.Trie Segment (EHandler r, W.State, W.Writer)
build mc domain r m = Tr.fromList list
  where
    list = eval mc def domain r m <&> _1 %~ tail . Re.toTextList

-- * Run

handle
  :: W.Conf
  -> r
  -> Wai.Request
  -> (EHandler r, W.State, W.Writer)
  -> IO Re.AnyResponse
handle mc r req (i_io, js_css_st, js_css_wr) = merge <$> res
  where
    res = runM mc r js_css_st (i_io req)
    merge (Re.HtmlDocument doc, st, wr) = Re.HtmlDocument $ collapse (js_css_wr <> wr) doc
    merge (other, _, _) = other

    collapse :: W.Writer -> W.Document -> W.Document
    collapse code doc
      = doc
      & add (W.style $ W.raw $ render () $ code ^. W.cssCode)
      & add (W.script $ W.raw $ render (mc^.W.jsConf.JS.renderConf) $ code ^. W.jsCode)
      where add w = W.head' %~ (>> w)

-- * To handler

toHandler
  :: forall r. (W.HasBrowser r W.Browser, HasDynPath r [Segment])
  => W.Conf -> URL -> r -> T r -> Wai.Request -> IO (Maybe Re.AnyResponse)
toHandler mc domain conf0 site req = traverse (handle mc runtimeConf req) res
  where
    app = build mc domain conf0 site
    path = Wai.pathInfo req :: [Segment]
    res :: Maybe (EHandler r, W.State, W.Writer)
    (res, runtimeConf) = case Tr.lookupPrefix path app of
      Just (pathSuffix, maybeValue, _) -> (maybeValue, conf0 & dynPath .~ pathSuffix)
      _ -> (Nothing, conf0)


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

api :: (MonadState [Segment] m, MonadReader URL m, MonadWriter [(Segment, T r)] m)
  => RWST URL (Writer r) State (M Identity r) (EHandler r)
  -> m URL
api m = next >>= flip pin m

xhrPost' m = do
  url :: URL <- api m
  lift . W.js . fmap JS.Par . JS.func $ \data_ -> xhrPost (JS.ulit $ renderURL $ url) data_ []

-- | Add segment with api endpoint and return its full url
pin :: (MonadWriter [(Segment, T r)] m, MonadReader URL m)
  => Segment
  -> RWST URL (Writer r) State (M Identity r) (EHandler r)
  -> m URL
pin name m = name / T m *> nextFullWith name

page = api . return . (\response _ -> return response) . Re.page

next :: MonadState [Segment] m => m Segment
next = get >>= \(x : xs) -> put xs *> return x

nextFullWith :: MonadReader URL m => Segment -> m URL
nextFullWith top = ask <&> URL.segments <>~ [top]

class HasDynPath s a | s -> a where
  dynPath :: Lens' s a
  {-# MINIMAL dynPath #-}
