module X.Widgets where

import Prelude2 hiding (div, (.>), (.=))
import X
import qualified CSS
import DOM.JS
import qualified JS.Lib as JS

-- parrallax :: _
parrallaxContainer bgValue = do
  container <- styled div $ do
    display "block"
    backgroundImage bgValue
    backgroundAttachment "fixed"
    backgroundPosition "center"
    backgroundRepeat "no-repeat"
    -- @media all and (max-width: 778px) { background-position: right; }

  return container


-- | Sticky header

sticky scrollContainer elemId extraCss = do
  cls <- css $ pure ()
  cssRule (ssFrom elemId & classes <>~ [cls]) $ do
    position "fixed"
    CSS.top 0
    zIndex 1
    extraCss

  js $ onLoad $ do
    doc <- new scrollContainer
    menuElem <- new $ findBy elemId
    initialTop <- new $ menuElem !. "offsetTop"
    is <- new $ lit False
    f <- newf $ do
      let top = doc !. "scrollTop"
          should = top .> initialTop
      ifelse should
        (ifonly (JS.not is) $ do
            bare $ menuElem!. "classList" !// "add" $ lit $ static $ unClass cls
            is .= lit True
        )
        (ifonly is $ do
            bare $ menuElem!. "classList" !// "remove" $ lit $ static $ unClass cls
            is .= lit False
        )
    bare $ addEventListener (Cast window) Scroll f
