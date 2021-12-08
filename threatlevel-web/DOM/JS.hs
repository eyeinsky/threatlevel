{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module DOM.JS where

import X.Prelude as P hiding (id)
import qualified X.Prelude as P
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HM

import JS hiding (Raw)
import qualified JS.BuiltIns.Full as JS
import qualified JS.Syntax

import qualified DOM.Core as D
import qualified CSS as CSS
import HTML
import DOM.Event
import XML
import SVG hiding (onload, id)

getAttribute :: Expr b -> Expr a -> Expr c
getAttribute k e = call1 (e !. "getAttribute") k

setAttribute :: Expr b -> Expr b -> Expr a -> Expr c
setAttribute k v e = call (e !. "setAttribute") [k, v]

requestAnimationFrame :: Expr a -> Expr b
requestAnimationFrame f = call1 (window !. "requestAnimationFrame") f

documentWrite :: Expr b -> Expr c
documentWrite what = call1 (document !. "write") what

onLoad f = do
  f' <- newf f
  bare $ addEventListener (Cast window) Load f'

-- * Finding elements

class JSSelector a where
  jsSelectorFrom :: a -> Expr String
instance JSSelector (Expr String) where
  jsSelectorFrom a = a
instance {-# OVERLAPPABLE #-} CSS.SelectorFrom a => JSSelector a where
  jsSelectorFrom s = lit $ render' $ CSS.selFrom s

matches :: JSSelector a => a -> Expr e -> Expr Bool
matches s e = call1 (e !. "matches") (jsSelectorFrom s)

querySelector :: JSSelector a => a -> Expr e -> Expr D.Tag
querySelector s e = call1 (e !. "querySelector") (jsSelectorFrom s)

-- | Query selector which includes the root node
querySelector' :: JSSelector a => a -> Expr e -> Expr D.Tag
querySelector' selector root = (matches selector root .&& root) .|| querySelector selector root

querySelectorAll :: JSSelector a => a -> Expr e -> Expr [D.Tag]
querySelectorAll s e = call1 (e !. "querySelectorAll") (jsSelectorFrom s)

closest :: JSSelector s => s -> Expr D.Tag -> Expr D.Tag
closest s e = call1 (e !. "closest") (jsSelectorFrom s)

-- | The global find
class    FindBy a where findBy :: a -> Expr Tag
instance FindBy D.Id where
   findBy (D.Id id) = valueSelf id (docCall "getElementById")
instance FindBy D.Class where
   findBy (D.Class a) = case a of
     Static v -> call1 (document !. "getElementsByClassName") (lit v)
     Dynamic v -> Cast v
instance FindBy TagName where
   findBy (TagName a) = valueSelf a (docCall "getElementsByTagName")
instance FindBy (Expr D.Id) where
   findBy a = docCall' "getElementById" a
instance FindBy (Expr D.Class) where
   findBy a = docCall' "getElementsByClassName" a

instance FindBy (HTML Both) where
  findBy a
    | Just id <- P.join maybeId = findBy (D.Id id)
    | [cls] <- classes_ = findBy cls
    | _ : _ : _ <- classes_ = error "FindBy (HTML a): more than one class to find by"
    | [] <- classes_ = error "FindBy (HTML a): no classes to find by"
    | otherwise = error "FindBy (HTML a): nothing to find by"
    where
      maybeId = a ^? _Element._2.id
      classes_ = a ^. _Element._2.classes.P.to (map D.Class) :: [D.Class]

instance FindBy Html where
   findBy a = case execWriter a of
     e : _ -> findBy e
     _ -> error "FindBy Html: no html to find by"
instance FindBy (Html -> Html) where
  findBy a = case execWriter $ a $ pure () of
    e : _ -> findBy e
    _ -> error "FindBy (Html -> Html): no html to find by"

valueSelf :: D.Value -> (TS.Text -> Expr b) -> Expr b
valueSelf v f = case v of
  Static a -> f a
  Dynamic a -> Cast a

docCall' :: TS.Text -> Expr b -> Expr c
docCall' f a = call1 (document !. f) a

docCall :: ToExpr a => TS.Text -> a -> Expr c
docCall f a = docCall' f (lit a)

--

ea s e = e !. s

offsetHeight = ea "offsetHeight"
scrollHeight = ea "scrollHeight"

scrollTop = ea "scrollTop"
scrollBottom e = scrollTop e + offsetHeight e

offsetTop = ea "offsetTop"
offsetBottom e = ea "offsetTop" e + offsetHeight e

atTop el = scrollTop el .== 0 :: Expr Bool
atBottom el = scrollBottom el .>= scrollHeight el :: Expr Bool

getComputedStyle e = call1 (ex "getComputedStyle") e

childNodes e = e !. "childNodes"

-- * Modify DOM

timeStamp e = e !. "timeStamp"

appendChild :: Expr Tag -> Expr Tag -> Expr ()
appendChild a t = call1 (t !. "appendChild") a

insertBefore a b = call (parentNode b !. "insertBefore") [a, b]

replaceChild old new = call (parentNode old !. "replaceChild") [new, old]

removeChild :: Expr Tag -> Expr Tag -> Expr Tag
removeChild parent child = call1 (parent !. "removeChild") child

parentNode :: Expr Tag -> Expr Tag
parentNode e = e !. "parentNode"

setInnerHTML e x = innerHTML e .= x

innerHTML e = e !. "innerHTML"

createElement :: TagName -> Expr Tag
createElement tn = docCall' "createElement" $ lit $ unTagName tn

createTextNode :: Expr String -> Expr Tag
createTextNode txt = docCall' "createTextNode" txt

createDocumentFragment :: Expr DocumentFragment
createDocumentFragment = call0 (document !. "createDocumentFragment")

-- *** Text input

-- cursorPosition :: Expr Tag -> M JT.Number (Expr JT.Number)
cursorPosition e = do
      start <- let_ $ e !. "selectionStart"
      end <- const $ e !. "selectionEnd"
      let_ $ ternary (start .== end) (Cast start) (Cast Null)
   {- ^ Get caret position from textarea/input type=text

      IE not implemented, see here for how:
         http://stackoverflow.com/questions/1891444/cursor-position-in-a-textarea-character-index-not-x-y-coordinates

   -}

-- * From JS_API

-- ** XMLHttpRequest (Ajax)

-- Expr URL -> data -> (\ x -> M y z) -> M a b
-- doPost' a b c = call ajaxExpr ["post", a, b, c]
doPost' uri data_ cb = do
   aj <- newf $ ajaxExpr
   bare $ call aj [lit "POST", uri, data_, cb]
doGet' uri data_ cb = do
   aj <- newf $ ajaxExpr
   bare $ call aj [lit "GET", uri, data_, cb]

ajaxExpr meth uri data_ callback = do
   xhr <- const $ ex "new XMLHttpRequest()"
   ifonly (callback .!== Undefined) $ do
      wrap <- newf $ \(_ :: Expr ()) -> do
         text <- const $ xhr !. "responseText"
         json <- const $ fromJSON text
         bare $ call1 callback json
      xhr !. "onload" .= Cast wrap
   bare (call (xhr !. "open") [meth, uri, lit True])
   bare $ call1 (xhr !. "send") data_

xhrRaw :: Expr a -> Expr a -> Expr c -> Expr d -> JS.M r ()
xhrRaw meth uri data_ callback = do
  xhr <- const $ ex "new XMLHttpRequest()"
  ifonly (callback .!== Undefined) $ do
    xhr !. "onload" .= callback
  bare (call (xhr !. "open") [Cast meth, uri, lit True])
  bare $ call1 (xhr !. "send") data_

xhrJs :: JS.Syntax.Conf -> Expr a -> Expr a -> Expr c -> [Expr d] -> JS.M r ()
xhrJs rc meth uri data_ args = do
  wrap <- newf $ \(resp :: Expr ()) -> do
    let funcText = responseText resp
        argsText = lit $ runReader (JS.Syntax.unargs args) rc
    bare $ call1 (ex "eval") $ funcText + argsText
  xhrRaw meth uri data_ wrap

responseText :: Expr a -> Expr b
responseText resp = resp !. "target" !. "responseText"

xhrGet :: Conf -> Expr a -> [Expr d] -> M r ()
xhrGet rc uri args = xhrJs rc "GET" uri Undefined args

xhrPost :: Conf -> Expr a -> Expr c -> [Expr d] -> M r ()
xhrPost rc uri data_ args = xhrJs rc "POST" uri data_ args

-- ** DOM/Event

focus :: Expr a -> Expr c
focus e = call0 (e !. "focus")

blur :: Expr a -> Expr c
blur e = call0 (e !. "blur")

-- | Get char from keyboard event
eventKey :: Expr a1 -> M a2 ()
eventKey event = do -- from: http://unixpapa.com/js/key.html
   retrn $ let
         which = event !. "which" -- :: Expr J.Number
         from arg = call (ex "String" !. "fromCharCode") [ arg ]
         -- from which or keyCode
      in ternary (which .== ex "null")
      (from $ event !. "keyCode" ) -- old IE
      (ternary
         (  (which .!= lit 0)
        .&& event !. "charCode" .!= lit 0
        ) (from which {-all others-}) Null)

preventDefault :: Event e => Expr e -> Expr ()
preventDefault e = call0 (e !. "preventDefault")

mkEventListener :: Event e => TS.Text -> Expr Tag -> e -> [Expr b] -> Expr c
mkEventListener a el et li = call (el !. a) (etStr : li)
  where etStr = lit $ eventString et

addEventListener :: Event e => Expr Tag -> e -> Expr b -> Expr c
addEventListener el et handler = mkEventListener "addEventListener" el et [handler]

removeEventListener :: Event e => Expr Tag -> e -> Expr b -> Expr c
removeEventListener el et handler = mkEventListener "removeEventListener" el et [handler]

alert :: Expr a -> Expr b
alert x = call1 (ex "alert") x

-- * RenderJSM instances

mkAttrCommon :: Expr a -> TS.Text -> Attribute -> M r ()
mkAttrCommon e _ attr = case attr of
  On event expr ->
     bare $ addEventListener (Cast e) event expr
  Boolean k v -> e !. tsKebab k .= lit v
  _ -> error "mkAttrCommon: Should be handled elsewhere"

instance RenderJSM (HTML Both) where
  renderJSM html = case html of
    Element tn as children -> do
      t <- const $ createElement tn
      attrsJSM t mkAttr as
      ts :: [Expr Tag] <- mapM renderJSM children
      forM_ ts $ bare . flip appendChild t
      return t
    Text txt -> return $ createTextNode (lit txt)
    Raw txt -> do
      tmp <- const $ createElement "div"
      innerHTML tmp .= lit txt
      nodes <- const $ tmp !. "childNodes"
      frag <- fmap Cast $ const $ createDocumentFragment
      i <- let_ 0
      JS.for (JS.length nodes JS..> 0) $ do
        bare $ appendChild (nodes .! 0) frag
        i .+= 1
      return frag
    Dyn expr -> return (Cast expr)
    Embed a -> renderJSM a
    where
      mkAttr :: Expr a -> TS.Text -> Attribute -> JS.M r ()
      mkAttr e k attr = case attr of
        Data _ v -> e !. "dataset" !. tsKebab k .= lit v
        Custom _ v -> e !. tsKebab k .= lit v
        _ -> mkAttrCommon e k attr

createHtmls :: Html -> JS.M r (Expr DocumentFragment)
createHtmls m = do
  f <- const $ createDocumentFragment
  forM_ (execWriter m) $ \ html -> do
    e <- renderJSM html
    bare $ appendChild e (Cast f)
  return f

-- * Svg

instance  RenderJSM (XML SVG AttributeSet Both) where
  renderJSM xml = case xml of
    Element tagName as children -> do
      t <- const $ mkElem tagName
      attrsJSM t mkAttr as
      ts :: [Expr Tag] <- mapM renderJSM children
      forM_ ts $ bare . flip appendChild t
      return t
    Text txt -> return $ createTextNode (lit txt)
    Raw _ -> error "XML SVG AttributeSet Both: Raw not implemented"
    -- ^ fix: see implementation for HTML Both, would that work for svg too?
    Dyn expr -> return (Cast expr)
    Embed a -> renderJSM a
    where
      mkAttr :: Expr a -> TS.Text -> Attribute -> JS.M r ()
      mkAttr e k attr = case attr of
        Data _ v -> e & setAttr ("data-" <> k) v & bare
        Custom _ v -> case k of
          "xmlns" -> pure ()
          _ -> e & setAttr k v & bare
        _ -> mkAttrCommon e k attr
        where
          setAttr :: TS.Text -> Value -> Expr a -> Expr b
          setAttr k v e = call (e !. "setAttributeNS") [Null, lit k, lit v]
          -- ^ The regular setAttribute supposedly doesn't work in all browsers.
          -- https://stackoverflow.com/questions/7273500/how-to-create-an-attribute-in-svg-using-javascript

      ns = "http://www.w3.org/2000/svg"

      mkElem :: TagName -> Expr Tag
      mkElem tagName = call (document !. "createElementNS") [ns, lit $ unTagName tagName]

attrsJSM :: Expr Tag -> (Expr Tag -> TS.Text -> Attribute -> JS.M r ()) -> AttributeSet -> JS.M r ()
attrsJSM t mkAttr as = do
  maybe (return ()) (\id -> t !. "id" .= lit id) (as^.id)
  forM_ (HM.toList $ as^.attrs) $ uncurry $ mkAttr t
  forM_ (map (value2either) $ as^.classes) $ \cls -> do
     bare $ t !. "classList" !// "add" $ either lit P.id cls

-- * Helpers

deleteCookie :: Expr String -> JS.M r ()
deleteCookie name = do
  document !. "cookie" .= value
  where value = name + "=; expires=Thu, 01 Jan 1970 00:00:01 GMT;"

-- | Convert strict text kebab-case to camelCase
tsKebab :: TS.Text -> TS.Text
tsKebab k = TL.toStrict $ kebab2camel $ TL.fromStrict k
