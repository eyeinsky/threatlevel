module Server.API where

import Common.Prelude as P
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Lens qualified as LL
import Data.Text.Strict.Lens qualified as SL

import Blaze.ByteString.Builder qualified as BBB
import Data.Aeson qualified as Aeson
import Network.HTTP.Types qualified as Wai
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets qualified as WS
import Network.Wai.Internal qualified as Wai
import Network.WebSockets qualified as WS
import Text.Boomerang hiding ((.~))
import Text.Boomerang.Texts
import Text.Boomerang.Texts qualified as B

import HTML qualified
import Render
import URL
import HTML (Html, Document(Document))
import Web.DSL qualified as W
import Identifiers (identifierSource)
import Trie qualified as Tr

docBody = undefined

utf8text :: BS.ByteString -> Wai.Header
utf8text what = (Wai.hContentType, ("text/"<>what<>"; charset=UTF-8"))

ctJson :: Wai.Header
ctJson = (Wai.hContentType, "application/json; charset=UTF-8")


data AnyResponse where
  HtmlDocument :: Document -> AnyResponse
--  JS :: JS.Syntax.Conf -> JS.M r a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: BL.ByteString -> AnyResponse

instance Show AnyResponse where
  show _ = "AnyResponse"

declareFields [d|
  data Response
    = Response
      { responseCode :: Wai.Status
      , responseHeaders :: [Wai.Header]
      , responseBody :: AnyResponse
      }
    | WebSocket WS.ServerApp
    | File Wai.Status [Wai.Header] FilePath (Maybe Wai.FilePart)
  |]

instance ToRaw Response where
  toRaw r = case r of
    Response status origHeaders anyResponse -> httpResponse status (origHeaders <> headers) bl
      where
        (headers, bl) = case anyResponse of
          HtmlDocument (Document html') -> ([utf8text "html"], tl^.re LL.utf8)
            where
              tl = "<!DOCTYPE html>" <> render () html'
          -- JS conf mcode -> let
          --   ((_, code),_) = JS.runEmpty conf mcode
          --   in ([Hdr.javascript], render conf code^.re LL.utf8)
          JSON a -> ([ctJson], Aeson.encode a)
          Raw b -> ([], b)

    WebSocket _ -> P.error "ToRaw: Response(WebSocket) can't be converted to Raw"
    -- ^ fix: Figure out a better solution

    File status headers path maybePartInfo -> Wai.responseFile status headers path maybePartInfo

-- * Helpers

-- | Successful response with empty headers
resp200 :: AnyResponse -> Response
resp200 = Response (toEnum 200) []

htmlDoc :: Html -> Html -> Response
htmlDoc head body = resp200 $ HtmlDocument (Document $ HTML.html $ head >> body)

page :: Html -> Response
page html = resp200 $ HtmlDocument $ Document html

renderedPage :: BL.ByteString -> Response
renderedPage = resp200 . Raw

text :: TL.Text -> Response
text text = Response (toEnum 200) hs $ Raw (text^.re LL.utf8)
  where hs = [utf8text "plain"]

json :: Aeson.ToJSON a => a -> Response
json a = resp200 $ JSON a

httpError :: Wai.Status -> TL.Text -> Response
httpError code message = rawText code [(Wai.hContentType, "text/plain")] message

redirect :: URL -> Response
redirect url = redirectRaw $ renderURL url

redirectRaw :: TL.Text -> Response
redirectRaw url = rawText (toEnum 303) [th Wai.hLocation url] ""

redirect' :: Int -> URL -> Response
redirect' code url = rawText (toEnum code) [th Wai.hLocation $ renderURL url] ""

redirectRaw' :: Int -> BS.ByteString -> Response
redirectRaw' code url = rawText (toEnum code) [(Wai.hLocation, url)] ""

noRobots :: Response
noRobots = raw "text/plain"
  "User-agent: *\n\
  \Disallow: / \n"

-- ** Raw

rawBl :: Wai.Status -> [Wai.Header] -> BL.ByteString -> Response
rawBl status headers bl = Response status headers $ Raw bl

rawBS :: Wai.Status -> [Wai.Header] -> BS.ByteString -> Response
rawBS status headers bl = Response status headers $ Raw (bl^.from strict)

rawText :: Wai.Status -> [Wai.Header] -> Text -> Response
rawText status headers text = Response status headers $ Raw (text^.re LL.utf8)

raw :: Text -> Text -> Response
raw header text = Response (toEnum 200) [th Wai.hContentType header] $ Raw (text^.re LL.utf8)

th :: Wai.HeaderName -> TL.Text -> Wai.Header
th name textValue = (name, textValue^.strict.re SL.utf8)


type Raw = Wai.Response

class ToRaw a where
  toRaw :: a -> Raw

instance ToRaw Raw where
  toRaw = id

httpResponse :: Wai.Status -> [Wai.Header] -> BL.ByteString -> Wai.Response
httpResponse status headers body = Wai.responseBuilder status headers (BBB.fromLazyByteString body)

-- * Helpers

waiAddHeaders :: Wai.ResponseHeaders -> Wai.Response -> Wai.Response
waiAddHeaders hs r = case r of
   Wai.ResponseBuilder st hdrs builder -> Wai.ResponseBuilder st (hs <> hdrs) builder
   Wai.ResponseFile st hdrs path mFilePart -> Wai.ResponseFile st (hs <> hdrs) path mFilePart
   Wai.ResponseStream _ _ _ -> todo
   Wai.ResponseRaw _ _ -> todo

cacheForever :: Wai.Header
cacheForever = (Wai.hCacheControl, "max-age=31536000, public, immutable")

-- * Inner

type M m r = W.WebT (ReaderT r m)

runM :: W.Reader_ -> r -> W.State_ -> M m r a -> m (a, W.State_, W.Writer_)
runM r r' s m = m
  & coerce
  & (\m' -> W.runRaw m' r s)
  & flip runReaderT r'

-- * Endpoint

type InT' r a = RWST URL (Writer r) State (M IO r) a
type EHandler r = Wai.Request -> M IO r Response
type InT r = InT' r (EHandler r)
data T r where T :: InT r -> T r
data State = State [Segment] (Maybe WS.ServerApp)
type Writer r = [(Segment, T r)]
type HandlerTuple r = (EHandler r, W.State_, W.Writer_, Maybe WS.ServerApp)

(/) :: MonadWriter [(Segment, T r)] m => Segment -> T r -> m ()
segment / endpoint = tell [(segment, endpoint)]

runT :: URL -> T r -> M IO r (EHandler r, State, Writer r)
runT url (T m) = runRWST m url (State identifierSource Nothing)

webSocket :: WS.ServerApp -> InT' r ()
webSocket ws = do
  modify (\(State i _) -> State i $ Just ws)

-- * Build

type HandlePoint r = (URL, HandlerTuple r)
type Built r = Tr.Trie Segment (HandlerTuple r)

build :: forall r. W.Reader_ -> W.State_ -> URL -> r -> T r -> IO (Built r)
build mc ms rootUrl r m = Tr.fromList <$> list
  where
    rootPath = rootUrl ^. URL.segments
    list = eval mc ms rootUrl r m
      <&> fmap (_1 %~ drop (length rootPath) . view URL.segments)

    eval :: W.Reader_ -> W.State_ -> URL -> r -> T r -> IO [HandlePoint r]
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
  :: W.Reader_
  -> r
  -> Wai.Request
  -> HandlerTuple r
  -> IO Response
handle mc r req (i_io, js_css_st, js_css_wr, _) = merge <$> res
  where
    res :: IO (Response, W.State_, W.Writer_)
    res = runM mc r js_css_st (i_io req)
    merge (Response s h (HtmlDocument doc), _, wr) = Response s h $ HtmlDocument $ collapse (js_css_wr <> wr) doc
    merge (other, _, _) = other

    collapse :: W.Writer_ -> HTML.Document -> HTML.Document
    collapse _code doc = doc
      -- todo: Document is only a newtype over Html here
      -- & add (HTML.style $ HTML.raw $ render () $ code ^. W.cssCode)
      -- & add (HTML.script $ HTML.raw $ render (mc^.W.jsConf) $ code ^. W.jsCode)
      -- where add w = HTML.head' %~ (>> w)

-- * To handler

class HasDynPath s a | s -> a where
  dynPath :: Lens' s a
  {-# MINIMAL dynPath #-}

type Confy r = (HasDynPath r [URL.Segment])

toHandler
  :: forall r. Confy r
  => W.Reader_ -> W.State_ -> URL -> r -> T r
  -> IO (Wai.Request -> IO (Maybe Response))
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
      Right (handlerTuple@(_, _, _, ws :: Maybe WS.ServerApp), suffix) ->
        if WS.isWebSocketsReq req
        then return $ WebSocket <$> ws
        else
          let conf = conf0 & dynPath .~ suffix
          in handle mc conf req handlerTuple <&> Just
      Left _ -> pure Nothing

-- * API

type API :: (Type -> Type) -> Type -> Constraint
type API m r = (MonadState State m, MonadReader URL m, MonadWriter [(Segment, T r)] m)

currentUrl :: MonadReader URL m => m URL
currentUrl = ask

api :: API m r => InT r -> m URL
api m = next >>= flip pin m

xhrPost = undefined

-- xhrPost' m = do
--   rc <- askEnv
--   url :: URL <- api m
--   lift . fmap Par . func AnonFunc $ \data_ ->
--     xhrPost rc (JS.lit url) data_ []

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

-- * Boomerang

-- | Take url, a unparser, value, and unparse the value to the end of url
--
mkUrl :: URL.URL -> Boomerang e [TS.Text] () (r :- ()) -> r -> URL.URL
mkUrl url unparser value = url & URL.segments <>~ segm
  where segm = B.unparseTexts unparser value & fromJust

-- | Dyn
parseDyn
  :: (MonadReader s f, HasDynPath s [Segment])
  => Boomerang TextsError [Segment] () (r :- ())
  -> f (Either TextsError r)
parseDyn parser = asks (view dynPath) <&> parseTexts parser

renderDyn :: Boomerang e [Segment] () (r :- ()) -> r -> URL -> URL
renderDyn pp dt url = url & URL.segments <>~ fromJust (unparseTexts pp dt)
