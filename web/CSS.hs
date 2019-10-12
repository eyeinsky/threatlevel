module CSS
  ( module CSS.Internal
  , module CSS.Shorthands, prop
  , module DOM.Core
  , resetCSS
  , setBoxSizing
  , centerContent
  , keyframes, keyframes', keyframe, browser, selector
  , media, supports
  , DeclM, renderDecls
  , flexbox
  ) where

import X.Prelude

import Web.Browser

import CSS.Internal hiding
  ( tag, maybeId, pseudos
  )

import CSS.Monad
import CSS.Shorthands
import DOM.Core hiding (Value) -- don't export attribute value, but css value

import Render

renderDecls :: DeclM a -> Text
renderDecls dm = render () $ view decls $ execDeclM dm

-- * Useful styles

resetCSS :: [Rule]
resetCSS = run (TagName "body") no <> run (TagName "div") no
   where
      no = do
        prop "padding" $ px 0
        prop "margin" $ px 0

setBoxSizing :: [Rule]
setBoxSizing = run (TagName "html") (boxSizing "border-box") <> run anyTag inherit
  where
    inherit = boxSizing "inherit"

centerContent :: CSSM ()
centerContent = do
  display "flex"
  flexFlow "column nowrap"
  justifyContent "center"
  alignItems "center"

flexbox :: Value -> CSSM ()
flexbox how = do
  display "flex"
  flexFlow how
