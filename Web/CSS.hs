module Web.CSS
  ( module Web.CSS.Internal
  , module Web.CSS.Shorthands
  , module Web.HTML.Core
  , resetCSS
  , setBoxSizing
  ) where

import Prelude2

import Web.CSS.Internal hiding
  ( tag, maybeId, classes, pseudos
  )
import Web.CSS.Monad
import Web.CSS.Shorthands
import Web.HTML.Core

resetCSS :: [Rule]
resetCSS = run (TagName "body") no <> run (TagName "div") no
   where
      no = do
        prop "padding" $ px 0
        prop "margin" $ px 0

setBoxSizing :: [Rule]
setBoxSizing = run (TagName "html") (boxSizing "border-box") <> run any inherit
  where
    forAny = inherit >> before inherit >> after inherit
    inherit = boxSizing "inherit"
    any = selFrom $ TagName "*"
