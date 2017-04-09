module DOM.JS where

import Prelude2 hiding ((.=), Bool)
import Text.Exts
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer (execWriter)

import Web.Browser
import JS

import qualified DOM.Internal as D
import qualified Web.CSS as CSS
import Web.HTML.Core
import DOM.Event


-- * Objects

window :: Expr Window
window = ex "window"

document :: Expr Document
document = ex "document"

location :: Expr Location
location = window !. "location"

onloadIs :: Code () -> M r ()
onloadIs code = onload .= FuncDef [] code -- :: Code' -> Statement ()

onload = window !. "onload"

on :: (Event event)
   => Expr Tag                       -- When this element
   -> event                          --   .. has this event
   -> Expr (Expr event, Proxy ()) --   .. then do this.
   -> M r ()
on e t f = onEvent .= f
  where onEvent = e !. toOn t

on' :: (Arguments a ~ (Expr event, Proxy ()), Event event, Function a)
  => Expr Tag
  -> event
  -> a
  -> M parent ()
on' e t f = do
   fdef <- func f
   on e t fdef

-- * Finding elements

-- | The global find
class    FindBy a where findBy :: a -> Expr Tag
instance FindBy CSS.Id where
   findBy (CSS.Id id) = valueSelf id (docCall "getElementById")
instance FindBy CSS.Class where
   findBy (CSS.Class a) = valueSelf a (docCall "getElementsByClassName")

instance FindBy CSS.TagName where
   findBy (CSS.TagName a) = valueSelf a (docCall "getElementsByTagName")
instance FindBy (Expr CSS.Id) where
   findBy a = docCall' "getElementById" a
instance FindBy (Expr CSS.Class) where
   findBy a = docCall' "getElementsByClassName" a

valueSelf :: D.Value -> (TL.Text -> Expr b) -> Expr b
valueSelf v f = case v of
  Static a -> f a
  Dynamic a -> Cast a

valueExpr :: D.Value -> Expr ()
valueExpr v = case v of
  Static a -> ulit a
  Dynamic a -> Cast a

docCall' f a = call1 (document !. f) a
docCall f a = docCall' f (ulit a)

-- |
findUnder :: FindBy a => Expr Tag -> a -> Expr Tag
findUnder e a = u



-- * Modify DOM

appendChild :: Expr Tag -> Expr Tag -> Expr ()
appendChild a t = call1 (t !. "appendChild") a

remove :: Expr Tag -> M r (Expr ())
remove e = browser <&> \b -> case b of
  IE -> call1 (parentNode e !. "removeChild") e
  _ -> call0 (e !. "remove")

parentNode :: Expr Tag -> Expr Tag
parentNode e = e !. "parentNode"

setInnerHTML e x = e !. "innerHTML" .= x

createElement :: TagName -> Expr Tag
createElement tn = docCall' "createElement" $ valueExpr $ unTagName tn

createTextNode :: Expr a -> Expr b
createTextNode txt = docCall' "createTextNode" txt

createDocumentFragment :: Expr DocumentFragment
createDocumentFragment = call0 (document !. "createDocumentFragment")

createClasses :: [Class] -> Expr ()
createClasses cs = if null dynamics'
  then statics
  else dynamics
  where
    (statics', dynamics') = partitionEithers $ map (value2either . unClass) cs
    statics = ulit $ TL.unwords statics'
    dynamics = JS.join " " $ ulit dynamics'

createHtml' :: HTML -> M r (Expr Tag)
createHtml' html = case html of
   TagNode tn mid cls attrs children -> do
      t <- new $ createElement tn
      maybe (return ()) (\id -> t !. "id" .= valueExpr (unId id)) mid
      forM_ (HM.toList attrs) $ \ (k,v) -> t !. k .= ulit v
      when (not . null $ cls) $
         t !. "className" .= createClasses cls
      ts :: [Expr Tag] <- mapM createHtml' children
      forM_ ts $ bare . flip appendChild t
      return t
   TextNode txt -> return $ createTextNode (ulit txt)
   JSNode expr -> return (Cast expr)

createHtml :: HTML -> Expr Tag
createHtml tr = FuncDef [] . eval $ createHtml' tr >>= retrn

createHtmls' :: HTMLM () -> M r (Expr DocumentFragment)
createHtmls' htmlm = do
  f <- new $ createDocumentFragment
  forM_ (execWriter htmlm) $ \ html -> do
    e <- createHtml' html
    bare $ appendChild e (Cast f)
  return f

createHtmls :: HTMLM () -> Expr Tag
createHtmls htmlm = FuncDef [] . eval $ createHtmls' htmlm >>= retrn

-- *** Text input

-- cursorPosition :: Expr Tag -> M JT.Number (Expr JT.Number)
cursorPosition e = do
      start <- new $ e !. "selectionStart"
      end <- new $ e !. "selectionEnd"
      new $ ternary (start .== end) (Cast start) (Cast Null)
   {- ^ Get caret position from textarea/input type=text

      IE not implemented, see here for how:
         http://stackoverflow.com/questions/1891444/cursor-position-in-a-textarea-character-index-not-x-y-coordinates

   -}


-- ** CSS

cssAttr e k v = e !. "style" !. k .= v
addClass cls el = bare $ call1 (el !. "classList" !. "add"   ) cls
remClass cls el = bare $ call1 (el !. "classList" !. "remove") cls

-- * From JS_API

-- ** XMLHttpRequest (Ajax)

-- Expr URL -> data -> (\ x -> M y z) -> M a b
-- doPost' a b c = call ajaxExpr ["post", a, b, c]
doPost' a b c = do
   aj <- newf $ ajaxExpr
   bare $ call aj [ulit "POST", a, b, c]
doGet' a b c = do
   aj <- newf $ ajaxExpr
   bare $ call aj [ulit "GET", a, b, c]

ajaxExpr meth uri data_ callback = do
   xhr <- new $ ex "new XMLHttpRequest()"
   ifonly (callback .!== Undefined) $ do
      wrap <- newf $ \(ret :: Expr ()) -> do
         text <- new $ xhr !. "responseText"
         json <- new $ fromJSON text
         bare $ call1 callback json
      xhr !. "onload" .= Cast wrap
   bare (call (xhr !. "open") [meth, uri, ulit True])
   bare $ call1 (xhr !. "send") data_

xhr meth uri data_ callback = do
   xhr <- new $ ex "new XMLHttpRequest()"
   ifonly (callback .!== Undefined) $ do
      wrap <- newf $ \(ret :: Expr ()) -> do
         text <- new $ xhr !. "responseText"
         json <- new $ fromJSON text
         bare $ call1 callback json
      xhr !. "onload" .= Cast wrap
   bare (call (xhr !. "open") [meth, uri, ulit True])
   bare $ call1 (xhr !. "send") data_



-- ** DOM/Event

-- focus :: Expr Tag -> Expr M r ()
focus e = call0 (e !. "focus")

-- blur :: Expr Tag -> M r ()
blur e = call0 (e !. "blur")

-- | Get char from keyboard event
eventKey event = do -- from: http://unixpapa.com/js/key.html
   retrn $ let
         which = event !. "which" -- :: Expr J.Number
         from arg = call (ex "String" !. "fromCharCode") [ arg ]
         -- from which or keyCode
      in ternary (which .== ex "null")
      (from $ event !. "keyCode" ) -- old IE
      (ternary
         (  (which .!= ulit 0)
        .&& event !. "charCode" .!= ulit 0
        ) (from which {-all others-}) Null)

preventDefault :: Event e => Expr e -> Expr ()
preventDefault e = call0 (e !. "preventDefault")

-- * Helpers

onloadDo :: M b a -> M b ()
onloadDo js = (onload .=) =<< block js
