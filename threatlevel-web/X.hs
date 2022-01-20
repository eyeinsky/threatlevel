{-# OPTIONS_GHC -Wno-orphans #-}
module X
  ( module X
  , module Export
  ) where

import X.Prelude as Export hiding
  ( get, split, last, set
  -- used in HTML
  , option, aside, pre, max, min, head
  , un, State, Writer, eq
  -- used in JS
  , forOf, break, not
  -- used in CSS
  , first
  , state
  , (<+>) )

import HTML as Export hiding
  -- used in Prelude
  ( id
  -- redefined here
  , href, src, for, favicon, html, action, style
  -- used in URL
  , param,
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
  , disabled, readOnly, required, default_, scope, link, placeholder
  -- defined in JS
  , empty
  -- defined in URL
  , host
  )
import Web.Monad as Export
import JS.Event as Export
import DOM.Event as Export

import URL as Export hiding (base)

import DOM as Export hiding (
  -- used in CSS
  Value, focus,
  -- used in X
  deleteCookie,
  -- defined both in HTML and DOM.Core
  Document
  )

import JS as Export hiding
  -- todo: describe these
  ( dir, for, run, Conf, String, State, concat
  -- internal
  , next
  )

import X.Wai as Export

import Web.CSS as Export

import Server as Export hiding
  -- used in Prelude
  ( error
  -- used in HTML
  , text, body, code, Raw
  -- used in Server.API
  , State, Writer, (/)
  -- used in JS
  , M
  )

import WebAPIs.DOM as Export

import qualified Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time

import Control.Concurrent

import qualified Web.Cookie as Wai
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified HTTP.Response as HR

import qualified Prelude as P
import X.Prelude as P
import JS hiding (String)

import CSS.DSL (DM)
import qualified URL
import qualified HTML
import qualified JS.Event
import qualified DOM
import qualified Server.Response as WR

-- * DOM.Event

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

jsonBody :: Expr a -> [(String, Expr String)]
jsonBody = jsonPayload

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

includeJs :: URL.URL -> Html
includeJs url = script ! src url $ "" ! Custom "defer" "true"

includeJs' :: TS.Text -> Html
includeJs' url = script ! HTML.src (Static url) $ "" ! Custom "defer" "true"

-- | Include JavaScript module from @url@ [api]
moduleSrc :: URL -> Html
moduleSrc url = HTML.script "" ! src url ! type_ "module"

-- | Include JavaScript module from untyped @url@ [api]
moduleSrc_ :: TS.Text -> Html
moduleSrc_ url = HTML.script "" ! HTML.src (Static url) ! type_ "module"

-- | Helper to turn attribute into URL
urlAttr :: URL.URL -> DOM.Value
urlAttr url = Static $ TL.toStrict $ render' url

src :: URL.URL -> Attribute
src url = HTML.src (urlAttr url)

action :: URL.URL -> Attribute
action url = HTML.action (urlAttr url)

for :: Id -> Attribute
for id = HTML.for (coerce id)

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

clearContent :: Expr Tag -> Expr ()
clearContent element = Assign (element !. "innerHTML") ""

-- | Replace content of the @element@ with @fragment@. Done in such a
-- way to be an expression.
replaceContent :: Expr DocumentFragment -> Expr Tag -> Expr ()
replaceContent fragment element = Par (clearContent element) .|| (element !// "append" $ fragment)

-- | Get closest parent with data-* attribute. Partial
closestData :: TS.Text -> Expr Tag -> Expr a
closestData attr el = (el !// "closest" $ lit ("[data-" <> attr <> "]")) !. "dataset" !. attr

-- * JS + URL

instance ToExpr URL.URL where
  lit = renderURL ^ lit

-- * JS + CSS

instance ToExpr Id where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr Class where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr SimpleSelector where
  lit = lit . render'

addClass :: Class -> Expr a -> M r ()
addClass cls el = bare $ call1 (el !. "classList" !. "add") $ mkExpr cls

removeClass :: Class -> Expr a -> M r ()
removeClass cls el = bare $ call1 (el !. "classList" !. "remove") $ mkExpr cls

remClass :: Class -> Expr a -> M r ()
remClass = removeClass
{-# DEPRECATED remClass "Use `removeClass` instead." #-}

mkExpr :: Class -> Expr a
mkExpr = Cast . lit . static . coerce

-- | In JS set element's inline style to @declarations@ [api]
inlineStyle :: Expr tag -> DM () -> M r ()
inlineStyle element declarations = do
  forM_ (execWriter declarations) $ \(Declaration k v) -> let
    property = P.tsKebab2camel k
    in element !. "style" !. property .= lit (render' v)

-- * JS + HTML + URL

jsHref :: Expr a -> Attribute
jsHref url = HTML.href (Dynamic $ Cast url)

-- * HTTP.Request

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

instance ToHtml (Expr Tag) where
  toHtml a = HTML.dyn a
instance ToHtml (Expr DocumentFragment) where
  toHtml a = HTML.dyn a
instance ToHtml (Expr String) where
  toHtml a = HTML.dyn $ createTextNode a
instance ToHtml (Expr TS.Text) where
  toHtml a = HTML.dyn $ createTextNode $ Cast a
instance ToHtml (Expr Int) where
  toHtml e = dyn $ createTextNode $ toString e

html = to toHtml

-- * Endpoint

exec'
  :: (MonadReader s m, MonadWeb m, HasJsConf s JS.Conf)
  => (Code b -> M b a) -> JS.M b a -> m WR.Response
exec' f jsm = do
  code :: Code b <- js $ mkCode jsm
  conf <- js ask
  return $ WR.resp200 $ JS conf (f code)

-- | An anonymous function definition expression is returned
exec = exec' f
  where
    f = bare . Par . AnonFunc Nothing []

execCall = exec' f
  where
    f = bare . call0 . Par . AnonFunc Nothing []

noCrawling :: API m a => m URL
noCrawling = pin "robots.txt" $ return $ Prelude.const $ return $ WR.noRobots

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
styled :: (MonadWeb m, Exclamatable a Id) => a -> CSSM -> m a
styled elem rules = do
  id <- cssId rules
  return $ elem ! id

styleds :: (MonadWeb m, Exclamatable a Class) => a -> CSSM -> m a
styleds elem rules = do
  class_ <- css rules
  return $ elem ! class_

(/) :: URL.URL -> TS.Text -> URL.URL
url / tail = url & URL.segments <>~ [tail]

redirectToHttps :: URL -> IO ()
redirectToHttps url =
  void $ forkIO $ Warp.runSettings settings
    $ \_ respond -> redirect url & HR.toRaw & respond
  where
    settings = Warp.setPort 80 Warp.defaultSettings

-- * Request

-- textFormSubmission :: Request -> IO [(Maybe TL.Text, Maybe (Maybe TL.Text))]
formSubmission :: Wai.Request -> IO [(BL.ByteString, Maybe BL.ByteString)]
formSubmission req = do
  lb <- Wai.strictRequestBody req
  let pairs = BL8.split '&' lb :: [BL.ByteString] -- queryString
      f (k, v) = case v of
        "" -> (k, Nothing)
        _ -> (k, Just $ BL8.tail v)
      g = strict %~ Wai.urlDecode True
      decode (k, v) = (g k, g <$> v)
  return $ map (decode . f . BL8.break (== '=')) pairs

textFormSubmission :: Wai.Request -> IO [(TL.Text, Maybe TL.Text)]
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

-- * Date

-- | Formats dates as "in 2 days" etc.
--
-- Adapted from here: https://blog.webdevsimplified.com/2020-07/relative-time-format/
mkFormatFromNow :: Expr a -> M r (Expr Date -> Expr String)
mkFormatFromNow formatter = do
  divisions <- const (lit $ map (\(a, b) -> lit [a, b])
    [ (60, "seconds")
    , (60, "minutes")
    , (24, "hours")
    , (7, "days")
    , (4.34524, "weeks")
    , (12, "months")
    , (ex "Number" !. "POSITIVE_INFINITY", "years")
    ] :: Expr [(Double, String)])
  let amount a = a !- 0
      name a = a !- 1

  fn $ \date -> do
    duration <- let_ $ (date - call0 (New dateConstructor)) P./ 1000
    iterArray divisions $ \ix -> do
      division <- const $ divisions !- ix
      ifonly ((ex "Math" !// "abs" $ duration) .< amount division) $ do
        retrn $ call (formatter !. "format")
          [ ex "Math" !// "round" $ duration
          , name division ]
      duration ./= amount division
    retrn (Null :: Expr String)
