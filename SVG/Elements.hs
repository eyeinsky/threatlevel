module SVG.Elements where

import Pr
import Control.Monad.Writer
import HTML.Core
import TH
import XML

data SVG
type Svg = Writer [XML SVG AttributeSet] ()

concat <$> mapM (mk [t|Svg|] . view (from packed) . kebab2camel) [
  -- https://developer.mozilla.org/en-US/docs/Web/SVG/Element
    "a"
  , "altGlyph"
  , "altGlyphDef"
  , "altGlyphItem"
  , "animate"
  , "animateColor"
  , "animateMotion"
  , "animateTransform"
  , "audio"
  , "canvas"
  , "circle"
  , "clipPath"
  , "color-profile"
  , "cursor"
  , "defs"
  , "desc"
  , "discard"
  , "ellipse"
  , "feBlend"
  , "feColorMatrix"
  , "feComponentTransfer"
  , "feComposite"
  , "feConvolveMatrix"
  , "feDiffuseLighting"
  , "feDisplacementMap"
  , "feDistantLight"
  , "feDropShadow"
  , "feFlood"
  , "feFuncA"
  , "feFuncB"
  , "feFuncG"
  , "feFuncR"
  , "feGaussianBlur"
  , "feImage"
  , "feMerge"
  , "feMergeNode"
  , "feMorphology"
  , "feOffset"
  , "fePointLight"
  , "feSpecularLighting"
  , "feSpotLight"
  , "feTile"
  , "feTurbulence"
  , "filter"
  , "font"
  , "font-face"
  , "font-face-format"
  , "font-face-name"
  , "font-face-src"
  , "font-face-uri"
  , "foreignObject"
  , "g"
  , "glyph"
  , "glyphRef"
  , "hatch"
  , "hatchpath"
  , "hkern"
  , "iframe"
  , "image"
  , "line"
  , "linearGradient"
  , "marker"
  , "mask"
  , "mesh"
  , "meshgradient"
  , "meshpatch"
  , "meshrow"
  , "metadata"
  , "missing-glyph"
  , "mpath"
  , "path"
  , "pattern"
  , "polygon"
  , "polyline"
  , "radialGradient"
  , "rect"
  , "script"
  , "set"
  , "solidcolor"
  , "stop"
  , "style"
  , "svg"
  , "switch"
  , "symbol"
  , "text"
  , "textPath"
  , "title"
  , "tref"
  , "tspan"
  , "unknown"
  , "use"
  , "video"
  , "view"
  , "vkern"
  ]
