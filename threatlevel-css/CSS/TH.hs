module CSS.TH
  ( module CSS.TH
  , prop
  ) where

import Common.Prelude
import Language.Haskell.TH
import Data.Text.Multiline
import qualified Data.Text as TS
import Data.List

import Common.TH

import CSS.Syntax
import CSS.DSL

-- * Helpers

pseudoClassPlain :: TS.Text -> CSSF
pseudoClassPlain name =
  withDerivedSelector (mkSelectorMod pseudos PseudoClass name)

pseudoClassArgumented :: TS.Text -> TS.Text -> CSSF
pseudoClassArgumented name arg =
  withDerivedSelector (mkSelectorModArg pseudos PseudoClass name arg)

pseudoElementPlain :: TS.Text -> CSSF
pseudoElementPlain name =
  withDerivedSelector (mkSelectorMod pseudos PseudoElement name)

pseudoElementArgumented :: TS.Text -> TS.Text -> CSSF
pseudoElementArgumented name arg =
  withDerivedSelector (mkSelectorModArg pseudos PseudoElement name arg)

-- * TH-generators

declareCssProperty :: String -> DecsQ
declareCssProperty propName = let
  name = camelName propName
  in declareFn name [t| Value -> PolyProp |] [| prop $(stringE propName) |]

declarePseudoClass :: Either String String -> DecsQ
declarePseudoClass e = case e of
  Left name -> let name' = camelName name
   in declareFn name' [t| CSSF |] [| pseudoClassPlain $(stringE name) |]
  Right name -> let name' = camelName name
   in declareFn name' [t| TS.Text -> CSSF |] [| pseudoClassArgumented $(stringE name) |]

declarePseudoElement :: Either String String -> DecsQ
declarePseudoElement e = case e of
  Left name -> let name' = camelName name
    in declareFn name' [t| CSSF |] [| pseudoElementPlain $(stringE name) |]
  Right name -> let name' = camelName name
    in declareFn name' [t| TS.Text -> CSSF |] [| pseudoElementArgumented $(stringE name) |]

-- | Turn "some-prop-name" into "somePropName", but as already a TH pattern, i.e a lhs of a function definition.
camelName :: String -> Name
camelName name = mkName $ reserved_ $ kebab2camel name

camelNameP :: String -> Q Pat
camelNameP name = varP $ camelName name

-- | From left column of http://www.w3schools.com/cssref/default.asp
allProperties :: [String]
allProperties = filter (('@' /=) . head) $ words "align-content align-items align-self all animation animation-delay animation-direction animation-duration animation-fill-mode animation-iteration-count animation-name animation-play-state animation-timing-function backface-visibility background background-attachment background-blend-mode background-clip background-color background-image background-origin background-position background-repeat background-size border border-bottom border-bottom-color border-bottom-left-radius border-bottom-right-radius border-bottom-style border-bottom-width border-collapse border-color border-image border-image-outset border-image-repeat border-image-slice border-image-source border-image-width border-left border-left-color border-left-style border-left-width border-radius border-right border-right-color border-right-style border-right-width border-spacing border-style border-top border-top-color border-top-left-radius border-top-right-radius border-top-style border-top-width border-width bottom box-shadow box-sizing caption-side clear clip color column-count column-fill column-gap column-rule column-rule-color column-rule-style column-rule-width column-span column-width columns content counter-increment counter-reset cursor direction display empty-cells filter flex flex-basis flex-direction flex-flow flex-grow flex-shrink flex-wrap float font @font-face font-family font-size font-size-adjust font-stretch font-style font-variant font-weight gap grid grid-area grid-auto-columns grid-auto-flow grid-auto-rows grid-column grid-column-end grid-column-gap grid-column-start grid-gap grid-row grid-row-end grid-row-gap grid-row-start grid-template grid-template-areas grid-template-columns grid-template-rows place-items hanging-punctuation height justify-content justify-items @keyframes left letter-spacing line-height list-style list-style-image list-style-position list-style-type margin margin-bottom margin-left margin-right margin-top max-height max-width @media min-height min-width nav-down nav-index nav-left nav-right nav-up opacity order outline outline-color outline-offset outline-style outline-width overflow overflow-x overflow-y padding padding-bottom padding-left padding-right padding-top page-break-after page-break-before page-break-inside perspective perspective-origin position quotes resize right row-gap tab-size table-layout text-align text-align-last text-decoration text-decoration-color text-decoration-line text-decoration-style text-indent text-justify text-overflow text-shadow text-transform top transform transform-origin transform-style transition transition-delay transition-duration transition-property transition-timing-function unicode-bidi vertical-align visibility white-space width word-break word-spacing word-wrap z-index"

prep :: [String] -> [Either String String]
prep = map toEither

toEither :: String -> Either String String
toEither c = if isFun c
  then Right (reverse $ dropWhile (`elem` "()") $ reverse c)
  else Left c
  where
    isFun str
      | last str == ')' = True
      | otherwise = False

pseudoClasses :: [Either String String]
pseudoClasses = prep pseudoClasses'

pseudoElements :: [Either String String]
pseudoElements = prep pseudoElements'

pseudoClasses' :: [String]
pseudoClasses' = common ":" [unindent|
  :active
  --:any-link
  --:blank
  :checked
  --:current
  :default
  :defined
  --:dir()
  :disabled
  --:drop
  :empty
  :enabled
  :first
  :first-child
  :first-of-type
  --:fullscreen
  :future
  :focus
  --:focus-visible
  :focus-within
  --:has()
  :host
  --:host()
  :host-context()
  :hover
  :indeterminate
  :in-range
  :invalid
  --:is()
  :lang()
  :last-child
  :last-of-type
  :left
  :link
  --:local-link
  :not()
  :nth-child()
  --:nth-col()
  :nth-last-child()
  --:nth-last-col()
  :nth-last-of-type()
  :nth-of-type()
  :only-child
  :only-of-type
  :optional
  :out-of-range
  --:past
  --:placeholder-shown
  :read-only
  :read-write
  :required
  :right
  :root
  :scope
  --:state()
  :target
  --:target-within
  --:user-invalid
  :valid
  :visited
  --:where()
  |]

pseudoElements' :: [String]
pseudoElements' = common "::" [unindent|
  ::after (:after)
  -- ::backdrop
  ::before (:before)
  ::cue
  ::cue-region
  ::first-letter (:first-letter)
  ::first-line (:first-line)
  -- ::grammar-error
  ::marker
  -- ::part()
  ::placeholder
  -- ^ Enabled because supported
  ::selection
  ::slotted()
  -- ::spelling-error
  |]

-- | Filter and strip by prefix (i.e leave out comments), drop everything after space
common :: String -> String -> [String]
common prefix = catMaybes . map (stripPrefix prefix . untilSpace) . lines
  where
    untilSpace = takeWhile (/= ' ')
