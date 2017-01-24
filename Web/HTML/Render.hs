module Web.HTML.Render where

import Prelude2 hiding (concat, unwords)
import qualified Data.HashMap.Strict as HM
import Web.HTML.Core
import Render
import Control.Monad.Writer (execWriter)

instance Render HTML where
  renderM html = pure $ case html of
    TagNode n mId cs as htmls -> "<" <> tag <> attrs2str as' <> rest
      where
        tag = unTagName n
        rest = case htmls of
          _ : _ -> let sub = concat (map render htmls)
            in ">" <> sub <> "</" <> tag <> ">"
          _ -> "/>"
        as' = HM.unions [as, id,  classes]
        id = maybe HM.empty (HM.singleton "id" . unId) mId
        classes = null cs ? HM.empty $ HM.singleton "class" $ unwords $ map unClass cs
    TextNode tl -> escape tl
      where escape tl = tl
    JSNode tl -> "error: Can't render browser js in back-end!"
    where
      attrs2str = HM.foldrWithKey (\k v x -> x <> " " <> k <> "=" <> q v) ""
      q v = "'" <> v <> "'"

instance Render [HTML] where
  renderM = pure . concat . map render

instance Render (HTMLM ()) where
  renderM htmlm = pure . render . execWriter $ htmlm
