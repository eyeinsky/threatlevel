{-# OPTIONS_GHC -Wno-orphans #-}
module X
  ( module X
  , module Export
  ) where

import HTML as Export hiding (
  -- redefined here
  href, src, for, favicon, html, action, param, style,
  -- used in CSS
  em, font, content, Value,
  -- used in HTTP
  header,
  raw,
  -- used in JS.DSL
  var, method,
  -- conflict with DOM.Core
  Id, Class
  )

import CSS as Export hiding
  -- generic
  ( filter, all, not
  -- defined in HTML
  , disabled, readOnly, required, default_, scope, link
  -- defined in JS
  , empty
  -- defined in URL
  , host
  -- defined both in HTML and DOM.Core
  , Document
  )
import Web.Monad as Export
import JS.Event as Export
import DOM.Event as Export

import URL as Export hiding (base)
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
  dir, for, run, Conf, String, State
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
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent

import Web.Cookie as Wai
import Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import Rapid

import System.IO as IO

import qualified HTTP.Response as HR

import X.Prelude as P
import JS hiding (String)

import CSS.Monad (DM)
import qualified CSS.Syntax as CSS
import qualified URL
import qualified HTML
import qualified JS.Event
import qualified DOM
import qualified Web.Monad as WM
import qualified Web.Response as WR
import qualified Web.Endpoint as WE

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS

-- * DOM.Event

-- | Create inline on-event attribute
on :: JS.Event.Event e => e -> Expr a -> Attribute
on event handler = On event handler
  where
    -- JS.Syntax.EName (JS.Syntax.Name handler') = handler
    -- ^ todo: find a generic way to get the name, even for literal
    -- expressions.
{-# DEPRECATED on "Use `On` instead, or better yet add it with addEventListener." #-}

onEvent
  :: (JS.Event.Event e, Function h) => e -> Expr a -> h
  -> M r (Expr b) -- (Expr (JS.Type h))
onEvent eventType obj handler = do
  handler' <- async handler
  bare $ addEventListener (Cast obj) eventType handler'
  return $ Cast handler'

-- | Attach an event handler on document load
attachOnLoad
  :: (JS.Event.Event e, Function h) => e -> Expr a -> h
  -> M r (Expr b) -- (Expr (JS.Type h))
attachOnLoad type_ element handler = do
  handler' <- async handler
  lit <- func AnonFunc $ bare $ addEventListener (Cast element) type_ handler'
  bare $ addEventListener (Cast window) Load lit
  return $ Cast handler'


post url dt cb = DOM.xhrRaw "POST" (lit $ render' url) dt cb
get_ url dt cb = DOM.xhrRaw "GET" (lit $ render' url) dt cb

postJs rc url = DOM.xhrJs rc "POST" (lit $ render' url)
getJs rc url = DOM.xhrJs rc "GET" (lit $ render' url)


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

jsonPayload :: Expr a -> [(String, Expr String)]
jsonPayload data_ =
  [ ("body", toJSON data_)
  , ("headers", lit
      [(lit "Content-Type", "application/json")])
  ]

-- * HTML + URL

favicon :: URL.URL -> Html
favicon url = HTML.link ! rel "icon" ! href url

faviconSvg :: URL -> Html
faviconSvg url = link ! rel "icon" ! href url ! Custom "sizes" "any" ! type_ "image/svg+xml"

href :: URL.URL -> Attribute
href url = HTML.href (Static $ TL.toStrict $ render' url)

stylesheet :: URL.URL -> Html
stylesheet url = link ! rel "stylesheet" ! type_ "text/css" ! href url

stylesheet' :: TS.Text -> Html
stylesheet' url = link ! rel "stylesheet" ! type_ "text/css" ! HTML.href (Static url)

includeCss = stylesheet :: URL.URL -> Html
{-# DEPRECATED includeCss "Use `stylesheet` instead." #-}
includeCss' = stylesheet' :: TS.Text -> Html
{-# DEPRECATED includeCss' "Use `stylesheet'` instead." #-}

includeJs :: URL.URL -> Html
includeJs url = script ! src url $ "" ! Custom "defer" "true"

includeJs' :: TS.Text -> Html
includeJs' url = script ! HTML.src (Static url) $ "" ! Custom "defer" "true"

-- | Helper to turn attribute into URL
urlAttr :: URL.URL -> DOM.Value
urlAttr url = Static $ TL.toStrict $ render' url

src :: URL.URL -> Attribute
src url = HTML.src (urlAttr url)

action :: URL.URL -> Attribute
action url = HTML.action (urlAttr url)

for :: Id -> Attribute
for id = HTML.for (unId id)

-- * HTML + Date.Time

parseTextTime :: (Monad m, MonadFail m, ParseTime t) => String -> TS.Text -> m t
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

-- * JS + HTML (= DOM)

-- | Replace content of the @element@ with @fragment@. Done in such a
-- way to be an expression.
replaceContent :: Expr DocumentFragment -> Expr Tag -> Expr ()
replaceContent fragment element = Par (Assign (element !. "innerHTML") "") .|| (element !// "append" $ fragment)

-- | Get closest parent with data-* attribute. Partial
closestData :: TS.Text -> Expr Tag -> Expr a
closestData attr el = (el !// "closest" $ lit ("[data-" <> attr <> "]")) !. "dataset" !. attr

-- * JS + URL

instance ToExpr URL.URL where
  lit = renderURL ^ lit

-- * JS + CSS

instance ToExpr Id where
  lit = unId ^ render' ^ lit

instance ToExpr Class where
  lit = unClass ^ render' ^ lit

instance ToExpr SimpleSelector where
  lit s = lit
    $ (maybe mempty render' $ s^.CSS.tag)
    <> (maybe mempty render' $ s^.CSS.maybeId)
    <> (TL.concat $ map render' $ s^.P.classes)
    <> (TL.concat $ map render' $ s^.CSS.pseudos)

addClass :: Class -> Expr a -> M r ()
addClass cls el = bare $ call1 (el !. "classList" !. "add") $ mkExpr cls

removeClass :: Class -> Expr a -> M r ()
removeClass cls el = bare $ call1 (el !. "classList" !. "remove") $ mkExpr cls

remClass :: Class -> Expr a -> M r ()
remClass = removeClass
{-# DEPRECATED remClass "Use `removeClass` instead." #-}

mkExpr :: Class -> Expr a
mkExpr = Cast . lit . static . unClass

-- | In JS set element's inline style to @declarations@
inlineStyle :: Expr tag -> DM () -> M r ()
inlineStyle element declarations = do
  forM_ (execWriter declarations) $ \(Declaration k v) -> let
    property = TL.toStrict $ kebab2camel $ TL.fromStrict k
    in element !. "style" !. property .= lit (render' v)

-- * JS + HTML + URL

jsHref :: Expr a -> Attribute
jsHref url = HTML.href (Dynamic $ Cast url)

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

-- * Endpoint

getRenderConf = WM.getConf <&> view WM.jsConf

-- | Render JSM to Expr a within the current MonadWeb context.
evalJSM
  :: forall s m b a. (MonadReader s m, MonadWeb m)
  => JS.M b a -> m (Code b)
evalJSM jsm = do
  stWeb <- WM.getState
  let
    JS.State fresh used lib = stWeb^.WM.jsState
    ((_, code :: Code b), _) = JS.run fresh used lib jsm
  return code

exec'
  :: (MonadReader s m, MonadWeb m)
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

decls :: DeclM a -> Attribute
decls = decls
{-# DEPRECATED decls "Use `style` instead." #-}

style :: DeclM a -> Attribute
style decls = Custom "style" (Static $ TL.toStrict $ renderDecls decls)

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

type SiteType r a
  = (WE.Confy r, Default r)
  => WM.Conf
  -> WM.State
  -> URL
  -> Warp.Settings
  -> T r
  -> a

siteMain :: Maybe Warp.TLSSettings -> SiteType r (IO ())
siteMain maybeTls mc ms siteRoot settings site = do
  handler <- WE.toHandler mc ms siteRoot def site
  let handler' req respond = do
        r :: Maybe WR.Response <- handler req
        case r of
          Just r' -> case r' of
            r''@ (Response {}) -> respond $ HR.toRaw r''
            WebSocket ws ->
              WS.websocketsOr WS.defaultConnectionOptions ws
                 (error "This should never happen")
                 req respond
            file@ (File {}) -> respond $ HR.toRaw file
          _ -> do
            print $ "Path not found: " <> show (Wai.pathInfo req)
              <> ", URL: " <> TL.unpack (render' siteRoot)
            respond $ HR.toRaw $ htmlDoc "" "Page not found"
  case maybeTls of
    Just tls -> Warp.runTLS tls settings handler'
    _ -> Warp.runSettings settings handler'

type HotType k r a = (Ord k) => k -> SiteType r a

mkHotPrim :: Maybe Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
mkHotPrim maybeTls name mc ms siteRoot settings site = (hot, stop, main)
  where
    main = siteMain maybeTls mc ms siteRoot settings site
    (hot, stop) = mkHot name main

hotHttp :: HotType k r (IO (), IO (), IO ())
hotHttp = mkHotPrim Nothing

hotHttps :: Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
hotHttps tls = mkHotPrim (Just tls)

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

-- | Turn event handler to async iterator
iterEvent :: Event e => e -> Expr a -> M r (Expr b)
iterEvent eventType element = do

  let
    eventType' = lit $ eventString eventType :: Expr String
    wrap done value = lit
      [("done", done), ("value" :: TS.Text, value)]

  event <- let_ Null
  resolver <- let_ Null

  handler <- newf $ \ev -> do
    ifelse resolver
      (bare $ call1 resolver $ wrap false ev)
      (event .= ev)

  let pair = [eventType', Cast handler]

  next <- newf $ do
    executor <- newf $ \resolve -> do
      ifelse event
        (do bare $ call1 resolve $ wrap false event
            event .= Null)
        (resolver .= resolve)
    retrn $ newPromise executor

  bare $ call (element !. "addEventListener") pair

  it <- newf $ retrn $ ex "this"
  return_ <- newf $ do
    bare $ call (element !. "removeEventListener") pair
    bare $ call1 resolver $ wrap true Undefined
    retrn $ wrap true Undefined
  throw_ <- newf $ \err -> do
    retrn $ wrap true $ reject err
  const $ lit
    [ (ex "Symbol" !. "asyncIterator", it)
    , (lit "next", next)
    , (lit "return", return_)
    , (lit "throw", Cast throw_)
    ]

eventPromise
  :: forall a b r e. Event e
  => Expr a -> e -> (Expr b -> Expr Bool) -> M r (Expr b)
eventPromise el eventType p = do
  executor <- newf $ \resolve -> mdo
    let args = [lit $ eventString eventType, handler]
    handler <- newf $ \ev -> do
      ifonly (p ev) $ do
        bare $ call1 resolve ev
        bare $ call (el !. "removeEventListener") args
    bare $ call (el !. "addEventListener") args
  return $ newPromise executor
