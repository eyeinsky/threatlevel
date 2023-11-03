module Server.API where

import X.Prelude hiding (Reader, Writer, State, fail)
import Control.Monad.Except

import qualified URL
import URL (URL, Segment)
import qualified Web.Monad as W
import qualified HTML
import qualified Data.Text as TS
import Render
import Identifiers (identifierSource)

import qualified JS
import DOM

import qualified Server.Response as Re
import qualified Network.Wai as Wai
import qualified Trie as Tr

import Text.Boomerang.Texts
import Text.Boomerang hiding ((.~))
import qualified Text.Boomerang.Texts as B

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS

-- * Inner

type M m r = W.WebT (ReaderT r m)

runM :: W.Conf -> r -> W.State -> M m r a -> m (a, W.State, W.Writer)
runM mc r st m = m
  & W.runWebMT mc st
  & flip runReaderT r

-- * Endpoint

data State = State [Segment] (Maybe WS.ServerApp)
type Writer r = [(Segment, T r)]

(/) :: MonadWriter [(Segment, T r)] m => Segment -> T r -> m ()
segment / endpoint = tell [(segment, endpoint)]

type InT' r a = RWST URL (Writer r) State (M IO r) a
type InT r = InT' r (EHandler r)
data T r where T :: InT r -> T r

runT :: URL -> T r -> M IO r (EHandler r, State, Writer r)
runT url (T m) = runRWST m url (State identifierSource Nothing)

webSocket :: WS.ServerApp -> InT' r ()
webSocket ws = do
  modify (\(State i _) -> State i $ Just ws)

type HandlerTuple r = (EHandler r, W.State, W.Writer, Maybe WS.ServerApp)
type EHandler r = Wai.Request -> M IO r Re.Response

-- * Build

type HandlePoint r = (URL, HandlerTuple r)
type Built r = Tr.Trie Segment (HandlerTuple r)

build :: forall r. W.Conf -> W.State -> URL -> r -> T r -> IO (Built r)
build mc ms rootUrl r m = Tr.fromList <$> list
  where
    rootPath = rootUrl ^. URL.segments
    list = eval mc ms rootUrl r m
      <&> fmap (_1 %~ drop (length rootPath) . view URL.segments)

    eval :: W.Conf -> W.State -> URL -> r -> T r -> IO [HandlePoint r]
    eval mc js_css_st0 url (r :: r) (m :: T r) = do
      ((main, State _ ws, subs), js_css_st1, js_css_w) <- runM mc r js_css_st0 (runT url m)
      let
        self = (url, (main, js_css_st1, js_css_w, ws)) :: HandlePoint r

        re :: (Segment, T r) -> IO [HandlePoint r]
        re (segm, sub) = eval mc js_css_st1 (url & URL.segments <>~ [segm]) r sub

      result :: [HandlePoint r] <- do
        x <- mapM re (subs :: Writer r)
        return $ self : mconcat x
      return result

-- * Run

handle
  :: W.Conf
  -> r
  -> Wai.Request
  -> HandlerTuple r
  -> IO Re.Response
handle mc r req (i_io, js_css_st, js_css_wr, _) = merge <$> res
  where
    res = runM mc r js_css_st (i_io req)
    merge (Re.Response s h (Re.HtmlDocument doc), _, wr) = Re.Response s h $ Re.HtmlDocument $ collapse (js_css_wr <> wr) doc
    merge (other, _, _) = other

    collapse :: W.Writer -> HTML.Document -> HTML.Document
    collapse code doc
      = doc
      & add (HTML.style $ HTML.raw $ render () $ code ^. W.cssCode)
      & add (HTML.script $ HTML.raw $ render (mc^.W.jsConf) $ code ^. W.jsCode)
      where add w = HTML.head' %~ (>> w)

-- * To handler

class HasDynPath s a | s -> a where
  dynPath :: Lens' s a
  {-# MINIMAL dynPath #-}

type Confy r = (HasDynPath r [URL.Segment])

toHandler
  :: forall r. Confy r
  => W.Conf -> W.State -> URL -> r -> T r
  -> IO (Wai.Request -> IO (Maybe Re.Response))
toHandler mc ms rootUrl conf0 site = do
  app <- build mc ms rootUrl conf0 site
  return $ \req -> let
    handler' :: Except String (HandlerTuple r, [Segment])
    handler' = do
      (pathSuffix, maybeHandler, _) <- let
        path = Wai.pathInfo req :: [Segment]
        in Tr.lookupPrefix path app & maybe (throwError "Path not found") pure
      handler <- maybeHandler
        & maybe (throwError "No handler at path") pure
      return (handler, pathSuffix)
    in case runExcept handler' of
      Right (handlerTuple@ (_, _, _, ws :: Maybe WS.ServerApp), suffix) ->
        if WS.isWebSocketsReq req
        then return $ Re.WebSocket <$> ws
        else
          let conf = conf0 & dynPath .~ suffix
          in handle mc conf req handlerTuple <&> Just
      Left _ -> pure Nothing

-- * Dyn path

parseDyn
  :: (MonadReader s f, HasDynPath s [Segment])
  => Boomerang TextsError [Segment] () (r :- ())
  -> f (Either TextsError r)
parseDyn parser = asks (view dynPath) <&> parseTexts parser

renderDyn :: Boomerang e [Segment] () (r :- ()) -> r -> URL -> URL
renderDyn pp dt url = url & URL.segments <>~ fromJust (unparseTexts pp dt)

-- * API

type API :: (Type -> Type) -> Type -> Constraint
type API m r = (MonadState State m, MonadReader URL m, MonadWriter [(Segment, T r)] m)

currentUrl :: MonadReader URL m => m URL
currentUrl = ask

api :: API m r => InT r -> m URL
api m = next >>= flip pin m

xhrPost' :: (MonadState State (t m), MonadReader URL (t m),
                   MonadWriter [(Segment, T r)] (t m), MonadTrans t, W.MonadWeb m,
                   W.MonadWeb (t m)) =>
                  InT r -> t m (JS.Expr (c -> b))
xhrPost' m = do
  rc <- W.js ask
  url :: URL <- api m
  lift . W.js . fmap JS.Par . JS.func JS.AnonFunc $ \data_ ->
    xhrPost rc (JS.lit $ render' url) data_ []

-- | Add segment with api endpoint and return its full url
pin :: API m r => Segment -> InT r -> m URL
pin name m = name / T m *> nextFullWith name

next :: MonadState State m => m Segment
next = get >>= \(State (x : xs) ws) -> put (State xs ws) *> return x

nextFullWith :: MonadReader URL m => Segment -> m URL
nextFullWith top = ask <&> URL.segments <>~ [top]

-- * Helpers

staticResponse :: (Monad m1, Monad m2) => a -> m1 (p -> m2 a)
staticResponse response = return $ \_ -> return response

-- | Take url, a unparser, value, and unparse the value to the end of url
mkUrl :: URL.URL -> Boomerang e [TS.Text] () (r :- ()) -> r -> URL.URL
mkUrl url unparser value = url & URL.segments <>~ segm
  where segm = B.unparseTexts unparser value & fromJust
