module HTML.Render
  ( module HTML.Render
  , module Render
  ) where

import Pr hiding (concat, unwords, id, eq)
import Prelude2.Has (HasId(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL

import HTML.Core
import Render
import Control.Monad.Writer (execWriter)
import DOM.Event
import XML

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


instance Render HTML where
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

instance Render [HTML] where
  renderM = pure . concat . map render

instance Render Html where
  renderM htmlm = pure . render . execWriter $ htmlm
