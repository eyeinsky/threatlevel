module XML.Render where

import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer
import Pr hiding (eq, id, concat)
import Prelude2.Has (HasId(..))
import Render
import XML.Core
import DOM.Core
import DOM.Event

instance Render Attribute where
  renderM attr = pure $ case attr of
    Custom k v -> eq k v
    OnEvent et expr -> eq (toOn et) (render expr)
    Data k v -> eq ("data-" <> k) v

eq k v = k <> "=" <> q v
  where
    q v = "'" <> v <> "'"

instance Render AttributeSet where
  renderM attrSet = fmap TL.unwords $ (catMaybes [id',  cs] <>) <$> rest
    where
      id' :: Maybe TL.Text
      id' = (eq "id" . static . unId) <$> attrSet^.id
      cs :: Maybe TL.Text
      cs = let cs = attrSet^.classes
        in null cs
          ? Nothing
          $ Just $ eq "class" (TL.unwords $ map (static . unClass) cs)
      rest = mapM renderM $ HM.elems (attrSet^.attrs)

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
        tag = static $ unTagName n
        rest = case htmls of
          _ : _ -> let sub = concat (map render htmls)
            in ">" <> sub <> "</" <> tag <> ">"
          _ -> "/>"
    Text tl -> pure $ escape tl
      where escape tl = tl
      -- ^ todo: actually escape the text! Or not? Am I using this to inject random stuff?
    Dyn tl -> pure $ "error: Can't render browser js in back-end!"
    Embed a -> renderM a

instance Render [XMLA ns Both] where
  renderM = pure . concat . map render

instance Render (XMLM ns Both) where
  renderM htmlm = pure . render . execWriter $ htmlm

-- * Helper

renderRaw :: Render a => a -> Writer [XML ns b c] ()
renderRaw x = text (render x)
