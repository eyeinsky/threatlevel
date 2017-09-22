module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Control.Monad.Writer
import Data.Text.Lazy.Lens (utf8)

import Pr hiding (head)
import XML
import Render hiding (Conf)
import DOM.Core
import HTML.Core hiding ((!), M, map, embed)
import HTTP.Response (ToResponse(..), Response(..), utf8textHdr)

-- * Response

instance ToResponse Document where
  toResponse (Document h b) = Response 200 [utf8textHdr "html"] $ tl^.re utf8
    where
      html' = html (head h >> b)
      tl = "<!DOCTYPE html>" <> render () html'

instance Render Document where
  renderM (Document h b) = tl
    where
      html' = html (head h >> b)
      tl = ("<!DOCTYPE html>" <>) <$> (renderM html')
