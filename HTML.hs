module HTML
  ( module HTML
  , module HTML.Core
  , module HTML.Render
  , module HTML.Shorthands
  , module DOM.Core
  ) where

import Pr
import Data.Text.Lazy.Lens (utf8)
import DOM.Core
import HTML.Core hiding ((!), M)
import qualified HTML.Core as W
import HTML.Render
import HTML.Shorthands hiding (head, body, map)
import HTML.Shorthands as H
import HTTP.Response (ToResponse(..), Response(..), utf8textHdr)

-- *

renderRaw :: forall a. Render a => a -> Html
renderRaw x = W.text (render x)

-- * Response

instance ToResponse Document where
  toResponse (Document h b) = Response 200 [utf8textHdr "html"] $ tl^.re utf8
    where
      html = H.html (H.head h >> b)
      tl = "<!DOCTYPE html>" <> render html

instance Render Document where
  renderM (Document h b) = tl
    where
      html' = H.html (H.head h >> b)
      tl = ("<!DOCTYPE html>" <>) <$> (renderM html')

-- * Exclamatable

class Exclamatable e a where
  (!) :: e -> a -> e

instance Exclamatable Html W.Id where
  (!) e id = e W.! id_ id
instance Exclamatable (Html -> Html) W.Id where
  (!) e id = e W.! id_ id

instance Exclamatable Html [W.Class] where
  (!) e cs = e W.! cls_ cs
instance Exclamatable (Html -> Html) [W.Class] where
  (!) e cs = e W.! cls_ cs

instance Exclamatable Html W.Class where
  (!) e c = e W.! cls_ [c]
instance Exclamatable (Html -> Html) W.Class where
  (!) e c = e W.! cls_ [c]

instance Exclamatable Html W.Attribute where
  (!) e c = e W.! c
instance Exclamatable (Html -> Html) W.Attribute where
  (!) e c = e W.! c
