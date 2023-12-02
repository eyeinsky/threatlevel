{-# OPTIONS_GHC -Wno-orphans #-}
module DOM.JS
  {-# DEPRECATED "This module is being phased out, much of the functionality is in Web.Apis.DOM" #-}
  where

import Common.Prelude hiding (id)
import Common.Prelude qualified as P
import Common.Lens
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer
import XML.Core

import JS hiding (Raw)
import qualified JS.BuiltIns.Full as JS

import qualified DOM.Core as D
import HTML
import SVG hiding (onload, id)

import Web.Apis.DOM

getAttribute :: Expr b -> Expr a -> Expr c
getAttribute k e = call1 (e !. "getAttribute") k

setAttribute :: Expr b -> Expr b -> Expr a -> Expr c
setAttribute k v e = call (e !. "setAttribute") [k, v]

requestAnimationFrame :: Expr a -> Expr b
requestAnimationFrame f = call1 (window !. "requestAnimationFrame") f

-- * Finding elements

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
    where
      maybeId = a ^? _Element._2.id_
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

-- * Modify DOM

timeStamp :: Expr a -> Expr b
timeStamp e = e !. "timeStamp"

-- *** Text input

cursorPosition :: JS m => Expr a1 -> m (Expr a2)
cursorPosition e = do
      start <- let_ $ e !. "selectionStart"
      end <- const $ e !. "selectionEnd"
      let_ $ ternary (start .== end) (Cast start) (Cast Null)
   {- ^ Get caret position from textarea/input type=text

      IE not implemented, see here for how:
         http://stackoverflow.com/questions/1891444/cursor-position-in-a-textarea-character-index-not-x-y-coordinates

   -}

-- ** DOM/Event

-- | Get char from keyboard event
eventKey :: JS m => Expr a1 -> m ()
eventKey event = do -- from: http://unixpapa.com/js/key.html
   return_ $ let
         which = event !. "which" -- :: Expr J.Number
         from arg = call (ex "String" !. "fromCharCode") [ arg ]
         -- from which or keyCode
      in ternary (which .== ex "null")
      (from $ event !. "keyCode" ) -- old IE
      (ternary
         (  (which .!= lit 0)
        .&& event !. "charCode" .!= lit 0
        ) (from which {-all others-}) Null)

alert :: Expr a -> Expr b
alert x = call1 (ex "alert") x

-- * RenderJSM instances

mkAttrCommon :: JS m => Expr a -> TS.Text -> Attribute -> m ()
mkAttrCommon e _ attr = case attr of
  On event expr ->
     bare $ addEventListener (Cast e) event expr
  Boolean k v -> e !. tsKebab2camel k .= lit v
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
      tmp !. "innerHTML" .= lit txt
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
      mkAttr :: JS m => Expr a -> TS.Text -> Attribute -> m ()
      mkAttr e k attr = case attr of
        Data _ v -> e !. "dataset" !. tsKebab2camel k .= lit v
        Custom _ v -> e !. tsKebab2camel k .= lit v
        _ -> mkAttrCommon e k attr

createHtmls :: JS m => Html -> m (Expr DocumentFragment)
createHtmls m = do
  f <- const $ createDocumentFragment
  forM_ (execWriter m) $ \ html -> do
    e <- renderJSM html
    bare $ appendChild e (Cast f)
  return f

-- -- * Svg

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
      mkAttr :: JS m => Expr a -> TS.Text -> Attribute -> m ()
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
      mkElem tagName = call (document !. "createElementNS") [ns, lit $ coerce @_ @Value tagName]

attrsJSM
  :: JS m
  => Expr Tag -> (Expr Tag -> TS.Text -> Attribute -> m ()) -> AttributeSet -> m ()
attrsJSM t mkAttr as = do
  maybe (return ()) (\id -> t !. "id" .= lit id) (as^.id_)
  forM_ (HM.toList $ as^.attrs) $ uncurry $ mkAttr t
  forM_ (map (value2either) $ as^.classes) $ \cls -> do
     bare $ t !. "classList" !// "add" $ either lit P.id cls

-- * Helpers

deleteCookie :: JS m => Expr String -> m ()
deleteCookie name = do
  document !. "cookie" .= value
  where value = name + "=; expires=Thu, 01 Jan 1970 00:00:01 GMT;"
