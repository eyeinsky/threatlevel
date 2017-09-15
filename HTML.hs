module HTML
  ( module HTML
  , module HTML.Core
  , module HTML.Render
  , module HTML.Shorthands
  , module DOM.Core
  ) where

import Control.Monad.Writer
import Pr
import Data.Text.Lazy.Lens (utf8)
import DOM.Core
import HTML.Core hiding ((!), M)
import HTML.Render
import HTML.Shorthands hiding (head, body, map)
import HTML.Shorthands as H
import HTTP.Response (ToResponse(..), Response(..), utf8textHdr)
import XML as X

-- *

renderRaw :: Render a => a -> Writer [XML ns b] ()
renderRaw x = text (render x)

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
