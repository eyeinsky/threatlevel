module CSS
  ( module CSS.Internal
  , module CSS.Shorthands, prop
  , module HTML.Core
  , resetCSS
  , setBoxSizing
  , keyframes, keyframe, browser, selector
  , media
  ) where

import Prelude2

import Web.Browser

import CSS.Internal hiding
  ( tag, maybeId, classes, pseudos
  )
import CSS.Monad
import CSS.Shorthands
import HTML.Core hiding (Value)

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
