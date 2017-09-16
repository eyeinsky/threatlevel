module CSS
  ( module CSS.Internal
  , module CSS.Shorthands, prop
  , module DOM.Core
  , resetCSS
  , setBoxSizing
  , keyframes, keyframe, browser, selector
  , media
  , DeclM, renderDecls
  ) where

import Prelude2

import Web.Browser

import CSS.Internal hiding
  ( tag, maybeId, classes, pseudos
  )
import CSS.Monad
import CSS.Shorthands
import DOM.Core hiding (Value) -- don't export attribute value, but css value

import Render

renderDecls :: Browser -> DeclM a -> Text
renderDecls r dm = render $ view decls $ execDeclM r dm

-- * Useful styles

resetCSS :: Browser -> [Rule]
resetCSS b = run b (TagName "body") no <> run b (TagName "div") no
   where
      no = do
        prop "padding" $ px 0
        prop "margin" $ px 0

setBoxSizing :: Browser -> [Rule]
setBoxSizing b = run b (TagName "html") (boxSizing "border-box") <> run b anyTag inherit
  where
    forAny = inherit >> before inherit >> after inherit
    inherit = boxSizing "inherit"
