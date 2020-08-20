module CSS
  ( module CSS
  , module CSS.Syntax
  , prop
  , module DOM.Core
  , type CSSM

  , keyframes, keyframes', keyframe, browser, selector
  , media, supports
  , DeclM
  ) where

import qualified Data.Text.Lazy as TL

import X.Prelude

import Web.Browser

import CSS.Syntax hiding
  ( tag, maybeId, pseudos
  )

import CSS.Monad
import CSS.TH
import DOM.Core hiding (Value) -- don't export attribute value, but css value


import Render


-- | TH-generate all properties with @prop@
concat <$> mapM shorthand list

alpha a = rgba 0 0 0 a

hover = pseudo "hover"
before = pseudo "before"
after = pseudo "after"
focus = pseudo "focus"
active = pseudo "active"
visited = pseudo "visited"

nthChild :: Int -> CSSM () -> CSSM ()
nthChild n = pseudo str
  where
    n' = TL.pack (show n)
    str = "nth-child(" <> n' <> ")"

descendant = combinator Descendant
child = combinator Child
sibling = combinator Sibling
generalSibling = combinator GeneralSibling

anyTag :: Selector
anyTag = selFrom $ TagName "*"

anyChild :: CSSM () -> CSSM ()
anyChild = child ("*" :: TagName)

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

flexbox :: (HasDecls w [Declaration], MonadWriter w m) => Value -> m ()
flexbox how = do
  display "flex"
  flexFlow how
