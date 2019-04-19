module Web
   ( module CSS
   , module Web.CSS
   , module HTML
   , module Web.Browser
   , module Web.Monad
   , module Render
   , module Web
   ) where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import Pr
import XML

import CSS
import Web.CSS (reset)
import HTML hiding (M, Value, content, font, em, id)
import Web.Browser
import Web.Monad
import Render hiding (Conf)

-- * Inline styling

styleAttr :: TS.Text -> Attribute
styleAttr = Custom "style"

decls :: Browser -> DeclM a -> Attribute
decls browser = renderDecls browser ^ TL.toStrict ^ styleAttr
