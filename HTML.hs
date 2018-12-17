module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Control.Monad.Writer
import Data.Text.Lazy.Lens (utf8)

import qualified Network.HTTP.Types as WT

import Pr hiding (head)
import XML
import Render hiding (Conf)
import DOM.Core
import HTML.Core hiding ((!), M, map, embed, input)
import qualified HTML.Core as Core
import HTTP.Response (utf8textHdr)

-- * Response

instance Render Document where
  renderM (Document h b) = tl
    where
      html' = html (head h >> b)
      tl = ("<!DOCTYPE html>" <>) <$> (renderM html')

-- * Shorthands

input :: Html
input = Core.input $ pure ()
