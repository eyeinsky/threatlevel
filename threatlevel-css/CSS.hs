module CSS
  ( module CSS
  , module CSS.Syntax
  , module CSS.TH
  , type CSSM
  , keyframes', keyframe
  , media, supports
  , DeclM
  ) where

import Common.Prelude as P

import CSS.Syntax hiding
  ( tag, maybeId, pseudos
  )
import CSS.DSL
import CSS.TH
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

descendant, child, sibling, generalSibling
  :: SimpleSelectorFrom a => a -> CSSF
descendant = combinator Descendant
child = combinator Child
sibling = combinator Sibling
generalSibling = combinator GeneralSibling

renderDecls :: DeclM a -> Text
renderDecls dm = render () $ view decls $ execDeclM dm

-- * Useful styles

resetCSS :: [Rule]
resetCSS = rulesFor (tagSelector "body") no <> rulesFor (tagSelector "div") no
   where
      no = do
        prop "padding" $ px 0
        prop "margin" $ px 0

centerContent :: DeclarationsM
centerContent = do
  display "flex"
  flexFlow "column nowrap"
  justifyContent "center"
  alignItems "center"

flexbox :: Value -> DeclarationsM
flexbox how = do
  display "flex"
  flexFlow how

square :: Value -> DeclarationsM
square n = do
  width n
  height n

circle :: Value -> DeclarationsM
circle n = do
  square n
  borderRadius $ prc 50
