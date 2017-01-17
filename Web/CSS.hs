module Web.CSS
  ( module Web.CSS.Internal
  , module Web.CSS.Shorthands
  , module Web.HTML.Core
  , resetCSS
  ) where

import Web.CSS.Internal
import Web.CSS.Shorthands
import Web.HTML.Core

resetCSS = do
   rule (TagName "body") $ nopad >> nomarg
   rule (TagName "div") $ nopad >> nomarg
   where
      nopad = prop "padding" $ px 0
      nomarg = prop "margin" $ px 0
