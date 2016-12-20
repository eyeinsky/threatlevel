module DOM.JS where

import Prelude2 hiding ((.=), Bool)
import Text.Exts
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import JS.Monad
import JS.API
import JS.Ops_Untyped
import Web.CSS as CSS
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

on :: (Event event, ToOn event)
   => Expr Tag                       -- When this element
   -> event                          --   .. has this event
   -> Expr (Expr event, Proxy ()) --   .. then do this.
   -> M r ()
on e t f = onEvent .= f
  where onEvent = e !. Name (toOn t)

on' :: (Arguments a ~ (Expr event, Proxy ()), Event event, ToOn event, Function a)
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
   findBy (CSS.Id t) = docCall "getElementById" t
instance FindBy CSS.Class where
   findBy (CSS.Class a) = docCall "getElementsByClassName" a
instance FindBy CSS.TagName where
   findBy (CSS.TagName a) = docCall "getElementsByTagName" a
instance FindBy (Expr CSS.Id) where
   findBy a = docCall' "getElementById" a
instance FindBy (Expr CSS.Class) where
   findBy a = docCall' "getElementsByClassName" a

docCall' f a = call1 (document !. f) a
docCall f a = docCall' f (ulit a)

-- |
findUnder :: FindBy a => Expr Tag -> a -> Expr Tag
findUnder e a = u



-- * Modify DOM

appendChild :: Expr Tag -> Expr Tag -> Expr ()
appendChild t a = call1 (t !. "appendChild") a -- :: Expr a

remove :: Expr Tag -> M r ()
remove e = bare $ call0 (e !. "remove")

setInnerHTML e x = e !. "innerHTML" .= x

createElement :: TagName -> Expr Tag
createElement tn = docCall "createElement" $ unTagName tn

-- creates the expr to create the tree, returns top
createHtml :: HTML -> Expr Tag
createHtml tr = FuncDef [] . eval $ case tr of
   TagNode tn mid cls attrs children -> do
      t <- new $ createElement tn
      maybe (return ()) (\id -> t !. "id" .= lit (unId id)) mid
      forM_ (HM.toList attrs) $ \ (k,v) -> t !. Name (TL.toStrict k) .= ulit v
      when (not . null $ cls) $
         t !. "className" .= lit (TL.unwords $ map unClass cls)
      mapM_ (bare . appendChild t . call0 . createHtml) children
      retrn t
   TextNode txt -> retrn $ docCall "createTextNode" txt


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
