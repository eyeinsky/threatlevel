module Web.CSS
  ( module Web.CSS.Internal
  , module Web.CSS.Shorthands, prop
  , module Web.HTML.Core
  , resetCSS
  , setBoxSizing
  , keyframes, keyframe, browser, selector
  ) where

import Prelude2

import Web.Browser

import Web.CSS.Internal hiding
  ( tag, maybeId, classes, pseudos
  )
import Web.CSS.Monad
import Web.CSS.Shorthands
import Web.HTML.Core hiding (Value)

resetCSS :: Browser -> [Rule]
resetCSS b = run b (TagName "body") no <> run b (TagName "div") no
   where
      no = do
        prop "padding" $ px 0
        prop "margin" $ px 0

setBoxSizing :: Browser -> [Rule]
setBoxSizing b = run b (TagName "html") (boxSizing "border-box") <> run b any inherit
  where
    forAny = inherit >> before inherit >> after inherit
    inherit = boxSizing "inherit"
    any = selFrom $ TagName "*"
