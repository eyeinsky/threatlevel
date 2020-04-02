{-# OPTIONS_GHC -Wno-orphans #-}
module X
  ( module X
  , module Export
  ) where

import HTML as Export hiding (
  -- redefined here
  href, src, for, favicon, html, action, param,
  -- used in CSS
  em, font, content, Value,
  -- used in HTTP
  header,
  raw,
  -- used in JS.DSL
  var,
  -- conflict with DOM.Core
  Id, Class
  )
import CSS as Export hiding (
  -- generic
  filter, all, transform,
  -- defined both in HTML and DOM.Core
  Document
  )
import Web.Monad as Export
import DOM.Event as Export

import URL as Export hiding (T, base)
import Web.Endpoint as Export hiding (State, Writer, (/), M)

import DOM as Export hiding (
  -- used in CSS
  Value, focus,
  -- used in X
  deleteCookie,
  -- defined both in HTML and DOM.Core
  Document
  )

import JS as Export hiding (
  -- todo: describe these
  dir, for, runM, Conf, String, State
  )

import Web.Response as Export hiding (
  -- TODO: describe why I hide these
  text, error, page, js, body, code, Raw)

import X.Wai as Export

import Web.Browser as Export

import Web.CSS as Export


import Warp_Helpers as Export (getRequestBody)

import qualified Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.Text.Strict.Lens as LS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent

import Web.Cookie as Wai
import Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.Mime as Mime
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import Rapid

import System.Process as IO
import System.IO as IO
import Language.Haskell.TH

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR

import X.Prelude as P
import JS hiding (String)
import qualified JS.DSL.Internal as JS

import CSS.Monad (CSSM)
import qualified URL
import qualified HTML
import qualified DOM
import qualified Web.Monad as WM
import qualified Web.Response as WR
import qualified Web.Endpoint as WE

-- * Prelude

-- * DOM.Event

-- | Create inline on-event attribute
on :: DOM.Event e => e -> Expr a -> Attribute
on event handler = OnEvent event handler
  where
    -- JS.Syntax.EName (JS.Syntax.Name handler') = handler
    -- ^ todo: find a generic way to get the name, even for literal
    -- expressions.

onEvent
  :: (Event e, Function h) => e -> Expr a -> h
  -> M r (Expr (JS.Type h))
onEvent eventType obj handler = do
  handler' <- async handler
  bare $ addEventListener (Cast obj) eventType handler'
  return $ Cast handler'


post url dt cb = DOM.xhrRaw "POST" (lit $ WR.renderURL url) dt cb
get url dt cb = DOM.xhrRaw "GET" (lit $ WR.renderURL url) dt cb

postJs url = DOM.xhrJs "POST" (lit $ WR.renderURL url)
getJs url = DOM.xhrJs "GET" (lit $ WR.renderURL url)


type Opts a b = (IsString a, IsString b, ToExpr [(a, b)])
fetch :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetch url extra = call (ex "fetch") [ url, lit extra ]

fetchMethod
  :: Opts a b => b -> Expr URL -> [(a, b)] -> Expr c
fetchMethod method url extra = fetch url opts
  where
    opts =  [("method", method)] <> extra

fetchPost :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetchPost = fetchMethod "POST"

fetchPut :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetchPut = fetchMethod "PUT"

-- * HTML

href :: URL.URL -> Attribute
href url = HTML.href (Static $ TL.toStrict $ WR.renderURL url)

for :: Id -> Attribute
for id = HTML.for (unId id)

-- * HTML + Date.Time

parseTextTime :: (Monad m, ParseTime t) => String -> TS.Text -> m t
parseTextTime fmt inp =
  parseTimeM True defaultTimeLocale fmt str
  where
    str = TS.unpack inp

format
  :: (Profunctor p, Contravariant f, FormatTime t)
  => String -> Optic' p f t TL.Text
format str = to (formatTime defaultTimeLocale str ^ TL.pack)

htmlDate = format "%F".html
htmlTime = format "%F %T".html

-- * JS + URL

instance ToExpr URL.URL where
  lit = renderURL ^ lit

-- * JS + CSS

instance ToExpr Id where
  lit = unId ^ render' ^ lit

instance ToExpr Class where
  lit = unClass ^ render' ^ lit

-- * HTTP.Response

deleteCookie :: TS.Text -> WR.Response -> WR.Response
deleteCookie key = WR.headers %~ (Hdr.delC (TL.fromStrict key) :)

setCookie :: TS.Text -> TS.Text -> WR.Response -> WR.Response
setCookie k v = WR.headers %~ (setCookie (TL.fromStrict v))
  where
    setCookie :: TL.Text -> [Hdr.Header] -> [Hdr.Header]
    setCookie val = (Hdr.cookie' (TL.fromStrict k) val Nothing (Just []) Nothing :)

-- * HTTP.Request

hasCookie :: TS.Text -> TS.Text -> Wai.Request -> P.Bool
hasCookie k v = getCookie k ^ maybe False (v ==)

getCookie :: TS.Text -> Wai.Request -> Maybe TS.Text
getCookie k = requestCookies >=> lookup k

requestCookies :: Wai.Request -> Maybe Wai.CookiesText
requestCookies = Wai.requestHeaders
  ^ lookup Wai.hCookie
  ^ fmap Wai.parseCookiesText

postKvs req = do
  bs <- getRequestBody req
  return $ Wai.parseQueryText $ BL.toStrict bs

queryParam' :: TS.Text -> Request -> Maybe (Maybe TS.Text)
queryParam' name req = lookup name $ queryText req

queryParam :: TS.Text -> Request -> Maybe TS.Text
queryParam name req = join $ queryParam' name req

hasParam :: TS.Text -> Request -> Bool
hasParam name req = isJust $ lookup name $ queryText req

-- * HTML

-- ** Back-end

class ToHtml a where toHtml :: a -> Html
instance ToHtml P.Int where toHtml = show ^ TL.pack ^ text
instance ToHtml P.String where toHtml = TL.pack ^ text
instance ToHtml Char where toHtml = TL.singleton ^ text
instance ToHtml TS.Text where toHtml = TL.fromStrict ^ text
instance ToHtml TL.Text where toHtml = text
instance ToHtml URL.URL where toHtml = renderURL ^ text
instance ToHtml Html where toHtml a = a

-- ** Front-end

instance ToHtml (Expr DocumentFragment) where
  toHtml a = HTML.dyn a
instance ToHtml (Expr String) where
  toHtml a = HTML.dyn $ createTextNode a
instance ToHtml (Expr TS.Text) where
  toHtml a = HTML.dyn $ createTextNode $ Cast a

html = to toHtml

-- * URL

param' k = params . URL.un <>~ [(k, Nothing)]
param k v = params . URL.un <>~ [(k, Just v)]

-- * URL + HTML

includeCss' :: TS.Text -> Html
includeCss' url = link ! rel "stylesheet" ! type_ "text/css" ! HTML.href (Static url) $ pure ()

includeCss :: URL.URL -> Html
includeCss url = link ! rel "stylesheet" ! type_ "text/css" ! href url $ pure ()

includeJs :: URL.URL -> Html
includeJs url = script ! src url $ "" ! Custom "defer" "true"

-- | Helper to turn attribute into URL
urlAttr :: URL.URL -> DOM.Value
urlAttr url = Static $ TL.toStrict $ WE.renderURL url

src :: URL.URL -> Attribute
src url = HTML.src (urlAttr url)

action :: URL.URL -> Attribute
action url = HTML.action (urlAttr url)

favicon :: URL.URL -> Html
favicon url = HTML.link ! rel "icon" ! href url $ pure ()

-- * Endpoint

getRenderConf = WM.getConf <&> view (WM.jsConf. JS.renderConf)

-- | Render JSM to Expr a within the current MonadWeb context.
evalJSM
  :: forall s m b a. (MonadReader s m, JS.HasBrowser s JS.Browser, MonadWeb m)
  => JS.M b a -> m (Code b)
evalJSM jsm = do
  browser <- asks (view JS.browser)
  stWeb <- WM.getState
  conf' <- WM.getConf
  let
    state = stWeb^.WM.jsState
    conf = conf' ^. WM.jsConf.JS.renderConf
    ((_, code :: Code b), _) = JS.runM (JS.Conf browser True conf) state jsm
  return code

exec'
  :: (MonadReader s m, JS.HasBrowser s JS.Browser, MonadWeb m)
  => (Code b -> Expr x) -> JS.M b a -> m WR.Response
exec' f jsm = do
  code <- evalJSM jsm
  conf <- getRenderConf
  return $ WR.js conf $ f code

-- | An anonymous function definition expression is returned
exec = exec' f
  where
    f = Par . AnonFunc Nothing []

execCall = exec' f
  where
    f = call0 . Par . AnonFunc Nothing []

noCrawling = do
  pin "robots.txt" $ return $ Prelude.const $ return $ WR.raw "text/plain"
    [unindent|
      User-agent: *
      Disallow: /
      |]

-- * Serving static assets

-- | Serve source-embedded files by their paths. Note that for dev
-- purposes the re-embedding of files might take too much time.
statics' (pairs :: [(FilePath, BS.ByteString)]) = forM pairs $ \(path, bs) -> let
  mime = path^.LS.packed.to Mime.defaultMimeLookup.from strict & TL.decodeUtf8
  headers = [Hdr.contentType mime]
  response = WR.rawBl (toEnum 200) headers (bs^.from strict)
  path' = TS.pack path
  in (path,) <$> (WE.pin path' $ WE.staticResponse response)

-- | Generate endpoints for source-embedded files and return the html
-- to include them.
includes (pairs :: [(FilePath, BS.ByteString)]) = statics' pairs <&> map f ^ sequence_
  where
    f :: (FilePath, URL.URL) -> Html
    f (path, url) = case P.split "." path^.reversed.ix 0 of
      "css" -> includeCss url
      "js" -> includeJs url
      _ -> pure ()

-- | Serve the subtree at fp from disk. The url is generated, the rest
-- needs to match file's path in the. TODO: resolve ".." in path and
-- error out if path goes outside of the served subtree. And check the
-- standard of if .. is even allowed in url paths.
staticDiskSubtree' mod notFound (fp :: FilePath) = do
  return $ \_ -> do
    e <- asks (view WE.dynPath) <&> sanitizePath
    e & either
      (\err -> do
          liftIO $ print err
          return notFound
      )
      (\subPath -> mod <$> WR.diskFile (fp <> "/" <> subPath))
  where
    sanitizePath :: [TS.Text] -> Either P.String P.String
    sanitizePath parts = if any (== "..") parts
      then Left "Not allowed to go up"
      else Right (TS.unpack $ TS.intercalate "/" parts)

-- | Serve entire path from under created url
staticDiskSubtree notFound path = staticDiskSubtree' P.id notFound path

-- | Serve files from filesystem path using a content adressable hash
assets notFound path = do
  hashPin path $ staticDiskSubtree' headerMod notFound path
  where
    headerMod = WR.headers <>~ [HR.cacheForever]
    hashPin path what = do
      hash <- liftIO (folderHash path) <&> TS.pack
      liftIO $ print (path, hash)
      WE.pin hash what

folderHash :: String -> IO [Char]
folderHash path = do
  (_, o, e, _) <- IO.runInteractiveCommand cmd
  IO.hGetContents e >>= hPutStrLn stderr
  IO.hGetContents o <&> P.take 40
  where
    cmd = "tar cf - '" <> path <> "' | sha1sum | cut -d ' ' -f 1"
    -- todo: better path escaping

folderHashTH :: FilePath -> ExpQ
folderHashTH path = runIO (folderHash path) >>= stringE

-- * Html + CSS

-- todo: The below could be more general!
getTag a = case execWriter a of
  e : _ -> let
      tn = e^?_Element._1 :: Maybe TagName
    in maybe (err "Not elem") ssFrom tn
  _ -> err "No xml content"
  where
    prefix = "SimpleSelectorFrom (XMLM ns c -> XMLM ns c): "
    err msg = error $ prefix <> msg

instance SimpleSelectorFrom (XMLM ns c -> XMLM ns c) where
  ssFrom a = getTag $ a $ pure ()

instance SimpleSelectorFrom (XMLM ns c) where
  ssFrom a = getTag a

fontSrc :: URL -> Maybe Text -> Value
fontSrc url mbFmt
  = Url (renderURL url) <> maybe "" f mbFmt
  where
    f fmt = Word $ "format(\"" <> fmt <> "\")"

styleAttr :: HTML.Value -> Attribute
styleAttr = Custom "style"

decls :: DeclM a -> Attribute
decls = renderDecls ^ TL.toStrict ^ Static ^ styleAttr

-- * Html + CSS + MonadWeb

-- | Generate id in the MonadWeb, apply styles to it, attach it to the
-- element and return this
-- todo: using exclamatable since this could be Html, Html -> Html, HTMLA Both, etc
styled :: (MonadWeb m, Exclamatable a Id) => a -> CSSM () -> m a
styled elem rules = do
  id <- cssId rules
  return $ elem ! id

styleds :: (MonadWeb m, Exclamatable a Class) => a -> CSSM () -> m a
styleds elem rules = do
  class_ <- css rules
  return $ elem ! class_

(/) :: URL.URL -> TS.Text -> URL.URL
url / tail = url & URL.segments <>~ [tail]

-- * Rapid

updated :: Ord k => k -> IO () -> IO ()
updated name main = rapid 1 (\r -> restart r name main)

mkHot :: Ord k => k -> IO () -> (IO (), IO ())
mkHot name what = let
  reload = updated name what
  stop_ = rapid 1 (\r -> stop r name what)
  in (reload, stop_)

siteMain
  :: (WE.Confy r, Default r)
  => WM.Conf
  -> WM.State
  -> URL
  -> Warp.Port
  -> Maybe Warp.TLSSettings
  -> T r
  -> IO ()
siteMain mc ms url port maybeTls site = do
  handler <- WE.toHandler mc ms url def site
  let handler' req respond = do
        r <- handler req
        r & fromMaybe err ^ HR.toRaw ^ respond
  case maybeTls of
    Just tls -> Warp.runTLS tls settings handler'
    _ -> Warp.runSettings settings handler'
  where
    settings = Warp.setPort port Warp.defaultSettings
    err = error "path not found"

hotHttp
  :: (WE.Confy r, Default r, Ord k)
  => k
  -> WM.Conf -> WM.State
  -> URL.URL
  -> Warp.Port
  -> WE.T r
  -> (IO (), IO (), IO ())
hotHttp name mc ms url port site = (hot, stop, main)
  where
    main = siteMain mc ms url port Nothing site
    (hot, stop) = mkHot name main

hotHttps
  :: (WE.Confy r, Default r, Ord k)
  => k
  -> WM.Conf -> WM.State
  -> URL.URL
  -> Warp.Port
  -> Warp.TLSSettings
  -> WE.T r
  -> (IO (), IO (), IO ())
hotHttps name mc ms url port tls site = (hot, stop, main)
  where
    main = siteMain mc ms url port (Just tls) site
    (hot, stop) = mkHot name main

redirectToHttps :: URL -> IO ()
redirectToHttps url =
  void $ forkIO $ Warp.runSettings settings
    $ \_ respond -> redirect url & HR.toRaw & respond
  where
    settings = Warp.setPort 80 Warp.defaultSettings

-- * Request

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

-- textFormSubmission :: Request -> IO [(Maybe TL.Text, Maybe (Maybe TL.Text))]
formSubmission :: Request -> IO [(BL.ByteString, Maybe BL.ByteString)]
formSubmission req = do
  lb <- strictRequestBody req
  let pairs = BL8.split '&' lb :: [BL.ByteString] -- queryString
      f (k, v) = case v of
        "" -> (k, Nothing)
        _ -> (k, Just $ BL8.tail v)
      g = strict %~ urlDecode True
      decode (k, v) = (g k, g <$> v)
  return $ map (decode . f . BL8.break (== '=')) pairs

textFormSubmission :: Request -> IO [(TL.Text, Maybe TL.Text)]
textFormSubmission req = do
  formSubmission req <&> map (f *** fmap f)
  where
    f = view LL.utf8

-- * Templating

-- | Creates ids, creates variables for the elements, returns a
-- function to bind them.
idsElems :: MonadWeb m => Int -> m ([Id], [Expr Tag], Expr b)
idsElems n = do
  ids <- replicateM n (cssId $ pure ())
  js $ do
    elems <- mapM (Prelude.const $ const Null) ids
    mount <- newf $ do
      forM (zip ids elems) $ \(id, el) -> el .= findBy id
    return (ids, elems, mount)

class Templating a where
  templating
    :: MonadWeb m
    => m
      -- ids: list of Id's that are attached to the html
      ( [Id]
      -- mount: find elements with ids and bind them
      , Expr ()
      -- create: js-create the html
      , Expr (a -> DocumentFragment)
      -- update: js-update the html
      , Expr (a -> ())
      -- ssr: render an empty or filled form
      , Maybe a -> Html
      -- get: get values from form
      , Expr a
      )
