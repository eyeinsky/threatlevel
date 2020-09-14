module CSS
  ( module CSS
  , module CSS.Syntax
  , prop
  , module DOM.Core
  , type CSSM

  , keyframes, keyframes', keyframe, browser, selector
  , media, supports
  , DeclM

  , module SVG.CSS
  ) where

import X.Prelude as P

import Web.Browser

import CSS.Syntax hiding
  ( tag, maybeId, pseudos
  )

import CSS.Monad
import CSS.TH
import DOM.Core hiding (Value) -- don't export attribute value, but css value

import SVG.CSS

import Render


-- | TH-generate all properties with @prop@
concat <$> mapM declareCssProperty allProperties

-- | TH-generate all pseudo-classes
$(
  let leaveOut = (`elem` ["left", "right"]) -- ^ These are also CSS properties
      predicate = not . either leaveOut leaveOut
  in concat <$> mapM declarePseudoClass (P.filter predicate pseudoClasses)
 )
-- | TH-generate all pseoudo-elements
concat <$> mapM declarePseudoElement pseudoElements

alpha a = rgba 0 0 0 a

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

important :: Value
important = "!important"
