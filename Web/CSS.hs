module Web.CSS
  ( module Web.CSS.Internal
  , module Web.CSS.Shorthands
  , module Web.HTML.Core
  , resetCSS
  , setBoxSizing
  ) where

import Prelude

import Web.CSS.Internal hiding (modifySelector, addPseoudo)
import Web.CSS.Shorthands
import Web.HTML.Core

resetCSS = do
   rule (TagName "body") $ nopad >> nomarg
   rule (TagName "div") $ nopad >> nomarg
   where
      nopad = prop "padding" $ px 0
      nomarg = prop "margin" $ px 0

setBoxSizing = do
  rule (TagName "html") $ boxSizing "border-box"
  inherit any
  inherit $ before any
  inherit $ after any
  where
    inherit s = rule s $ boxSizing "inherit"
    any = selFrom $ TagName "*"
