{-# OPTIONS_GHC -Wno-orphans #-}
module XML.Render where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer
import X.Prelude hiding (eq, id, concat)
import Render
import XML.Core
import DOM.Core hiding (Id(..), Class(..))
import JS.Event
import qualified JS.Syntax
import qualified JS

instance Render Attribute where
  renderM attr = case attr of
    Custom k v -> eq' k v
    On et handler -> pure $ let
      value = renderJS $ JS.call1 handler $ JS.ex "event"
      in eq (TL.fromStrict $ toOn et) value
    Data k v -> eq' ("data-" <> k) v
    Boolean k v -> if v
      then f k
      else error "False booleans shouldn't be here"
    Class _ -> todo
    Id _ -> todo
    where
      f = pure . TL.fromStrict
      eq' :: TS.Text -> Value -> Reader (Conf Value) TL.Text
      eq' k v = eq <$> f k <*> renderM v

eq k v = k <> "=" <> q v
  where
    q v = "'" <> v <> "'"

instance Render AttributeSet where
  renderM attrSet = fmap TL.unwords $ (catMaybes [id',  cs] <>) <$> rest
    where
      id' :: Maybe TL.Text
      id' = (eq "id" . TL.fromStrict . static) <$> attrSet^.id
      cs :: Maybe TL.Text
      cs = let cs = attrSet^.classes
        in null cs
          ? Nothing
          $ Just $ eq "class" (TL.unwords $ map (TL.fromStrict . static) cs)

      boolean = \case Boolean _ b -> b; _ -> True
      rest' = attrSet^.attrs & HM.elems & filter boolean
      rest = mapM renderM rest'

instance Render (XMLA ns Both) where
  renderM html = case html of
    Element n attrSet htmls -> TL.concat <$> sequence
      [ pure "<"
      , pure tag
      , f <$> renderM attrSet
      , pure rest
      ]
      where
        f attrText = TL.null attrText ? "" $ (" " <> attrText)
        tag = TL.fromStrict . static $ coerce n
        rest = case htmls of
          _ : _ -> let sub = TL.concat (map render' htmls)
            in ">" <> sub <> "</" <> tag <> ">"
          _ -> "/>"
    Text tl -> pure $ htmlTextEscape tl
    Raw tl -> pure tl
    Dyn _ -> pure $ "error: Can't render browser js in back-end!"
    Embed a -> renderM a

instance Render [XMLA ns Both] where
  renderM = pure . TL.concat . map render'

instance Render (XMLM ns Both) where
  renderM htmlm = pure . render' . execWriter $ htmlm

-- * Helper

renderRaw
  :: (Render a, Conf a ~ Conf (XML ns b c))
  => Conf a -> a -> Writer [XML ns b c] ()
renderRaw conf a = text (render conf a)

htmlTextEscapeMap :: [(Char, TL.Text)]
htmlTextEscapeMap = map (second $ \x -> "&" <> x <> ";") [
    ('&', "amp")
  , ('<', "lt")
  , ('>', "gt")
  , ('"', "quot")
  , ('\'', "#039")
  , ('/', "#x2F")
  ]
-- ^ https://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet#RULE_.231_-_HTML_Escape_Before_Inserting_Untrusted_Data_into_HTML_Element_Content

htmlTextEscape :: TL.Text -> TL.Text
htmlTextEscape t = foldl f t map'
  where
    f t (a, b) = TL.replace a b t
    map' = map (first TL.singleton) htmlTextEscapeMap

renderJS = render JS.Syntax.Minify
