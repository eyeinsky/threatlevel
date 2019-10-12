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

import X.Prelude
import XML

import CSS hiding (Document) -- todo
import Web.CSS (reset)
import HTML hiding (Value, content, font, em, id)
import Web.Browser
import Web.Monad
import Render hiding (Conf)

-- * Inline styling

styleAttr :: TS.Text -> Attribute
styleAttr = Custom "style"

decls :: DeclM a -> Attribute
decls = renderDecls ^ TL.toStrict ^ styleAttr
