module HTML.Render
  ( module HTML.Render
  , module Render
  ) where

import Prelude2 hiding (concat, unwords)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL

import HTML.Core
import Render
import Control.Monad.Writer (execWriter)
import DOM.Event

instance Render HTML where
  renderM html = pure $ case html of
    TagNode n mId cs as htmls -> "<" <> tag <> (TL.null as' ? "" $ (" " <> as')) <> rest
      where
        tag = static $ unTagName n
        rest = case htmls of
          _ : _ -> let sub = concat (map render htmls)
            in ">" <> sub <> "</" <> tag <> ">"
          _ -> "/>"
        as' = TL.unwords $ (v2s <$> HM.elems as) <> catMaybes [id,  classes]
        id = (eq "id" . static . unId) <$> mId
        classes = null cs ? Nothing $ Just $ eq "class" (TL.unwords $ map (static . unClass) cs)
    TextNode tl -> escape tl
      where escape tl = tl
    JSNode tl -> "error: Can't render browser js in back-end!"
    where
      v2s attr = case attr of
        Custom k v -> eq k v
        OnEvent et expr -> eq (toOn et) (render expr)
        Data k v -> eq ("data-" <> k) v
        where
      eq k v = k <> "=" <> q v
      q v = "'" <> v <> "'"

instance Render [HTML] where
  renderM = pure . concat . map render

instance Render (HTMLM ()) where
  renderM htmlm = pure . render . execWriter $ htmlm
