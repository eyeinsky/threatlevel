module JS_DOM where

import Prelude2
import Text.Exts
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import JS_Syntax as JS
import JS_Monad
import qualified JS_Types as JT
import JS_Ops_Untyped
import Web_CSS as CSS
import Web_HTML


window :: Expr Window
window = ex "window"

document :: Expr Document
document = ex "document"

location :: Expr Location
location = window !. "location"

onloadIs :: Code () -> M r ()
onloadIs code = onload .= FuncDef [] code -- :: Code' -> Statement ()

-- onload :: Expr 
onload = ex "window" !. "onload"




on :: (Event event, ToOn event)
   => Expr Tag                       -- When this element
   -> event                          --   .. has this event
   -> Expr (Expr event, JT.Proxy ()) --   .. then do this.
   -> M r ()
on el eventType fexpr = do
   (el !. Name (toOn eventType)) .= fexpr


-- * Finding elements

-- | The global find
class    FindBy a where findBy :: a -> JS.Expr Tag
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

createElement :: TagName -> JS.Expr Tag
createElement tn = docCall "createElement" $ unTagName tn

-- creates the expr to create the tree, returns top
createHtml :: HTML -> JS.Expr Tag
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


zepto expr = FuncCall (ex "$") [ expr ] -- :: Expr ()

setInnerHTML e x = e !. "innerHTML" .= x

-- ** CSS

cssAttr e k v = e !. "style" !. k .= v
addClass cls el = bare $ call1 (el !. "classList" !. "add"   ) cls
remClass cls el = bare $ call1 (el !. "classList" !. "remove") cls
