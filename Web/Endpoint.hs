module Web.Endpoint
  ( module Web.Endpoint
  , module Web.Response
  ) where


import Pr hiding (Reader, Writer, State, (/))

import Control.Monad.State (get, put)
import Data.DList as DList
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
import DOM

import qualified Web.Response as Re
import Web.Response (renderURL)
import qualified Network.Wai as Wai
import qualified Trie as Tr

import Text.Boomerang.Texts
import Text.Boomerang.TH
import Text.Boomerang hiding ((.~))


type I m r = W.WebT (ReaderT r m)

runI :: W.Conf -> r -> W.State -> I m r a -> m (a, W.State, W.Writer)
runI mc r st m = m
  & W.runWebMT mc st
  & flip runReaderT r

type Path = [Segment]

type State = [Segment]
type Writer r = [(Segment, T r)]

(/) :: MonadWriter [(Segment, T r)] m => Segment -> T r -> m ()
segment / endpoint = tell [(segment, endpoint)]

data T r where
  T :: RWST URL (Writer r) State (I Identity r) (I IO r Re.AnyResponse) -> T r
unT (T a) = a
runT url m = runRWST (unT m) url identifierSource

-- * Build

eval
  :: ( W.HasBrowser r W.Browser
     )
  => W.Conf -> W.State -> URL -> r -> T r -> [A r]
eval mc js_css_st0 up (r :: r) (m :: T r) = let
    ((main, _ {-stUrls-}, subs), js_css_st1, js_css_w)
      = runIdentity (runI mc r js_css_st0 (runT up m))

    self = (up, main, js_css_st1, js_css_w) :: A r

    re :: (Segment, T r) -> [A r]
    re (url, m') = eval mc js_css_st1 (up & URL.segments <>~ [url]) r m

  in self : (re =<< subs) :: [A r]

type IIO r = I IO r Re.AnyResponse
type A r = (URL, IIO r, W.State, W.Writer)

build
  :: ( W.HasBrowser r W.Browser
     )
  => W.Conf -> URL -> r -> T r -> [(Path, IIO r, W.State, W.Writer)]
build mc domain r m = eval mc def domain r m <&> \a -> a & _1 %~ Re.toTextList

-- * Run

run
  :: (Functor f)
  => W.Conf -> r -> (Path, I f r Re.AnyResponse, W.State, W.Writer) -> f Re.AnyResponse
run mc r (_, i_io, js_css_st, js_css_wr) = merge <$> res
  where
    res = runI mc r js_css_st i_io
    merge (Re.HtmlDocument doc, st, wr) = Re.HtmlDocument $ collapse (js_css_wr <> wr) doc
    merge (x, _, _) = x

    collapse :: W.Writer -> W.Document -> W.Document
    collapse code doc
      = doc
      & add (W.style $ W.raw $ render () $ code ^. W.cssCode)
      & add (W.script $ W.raw $ render (mc^.W.jsConf.JS.renderConf) $ putOnload $ code ^. W.jsCode)
      where add w = W.head' %~ (>> w)

-- * To handler

toHandler
  :: ( W.HasBrowser r W.Browser
     , HasDynPath r Path
     )
  => W.Conf -> URL -> r -> T r -> Wai.Request -> IO (Maybe Re.AnyResponse)
toHandler mc domain conf site req = traverse (run mc conf') res
  where
    app = build mc domain conf site
    found = Tr.lookupPrefix path $ Tr.fromList $ (\x -> (Pr.tail $ view _1 x, x)) <$> app
    (res, conf') = case found of
      Just (p, mv, _) -> (mv, conf & dynPath .~ p)
      _ -> (Nothing, conf)

    path = Wai.pathInfo req

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
  => RWST URL (Writer r) State (I Identity r) (I IO r Re.AnyResponse)
  -> m URL
api m = next >>= flip pin m

xhrPost m = do
  url :: URL <- api m
  lift . W.js . fmap JS.Par . JS.func $ \data_ -> xhrJs "post" (JS.ulit $ renderURL $ url) data_

-- | Add segment with api endpoint and return its full url
pin :: (MonadWriter [(Segment, T r)] m, MonadReader URL m)
  => Segment
  -> RWST URL (Writer r) State (I Identity r) (I IO r Re.AnyResponse)
  -> m URL
pin name m = name / T m *> nextFullWith name

page = api . return . return . Re.page

next :: MonadState [Segment] m => m Segment
next = get >>= \(x : xs) -> put xs *> return x

nextFullWith :: MonadReader URL m => Segment -> m URL
nextFullWith top = ask <&> URL.segments <>~ [top]

class HasDynPath s a | s -> a where
  dynPath :: Lens' s a
  {-# MINIMAL dynPath #-}
