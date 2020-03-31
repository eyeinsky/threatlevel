module X.Widgets where

import X.Prelude as P
import Prelude as P ((/))
import X
import qualified CSS
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
    doc <- const scrollContainer
    menuElem <- const $ findBy elemId
    initialTop <- const $ menuElem !. "offsetTop"
    is <- const $ lit False
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

-- | Fading background images

faidingImages li image ft = keyframes $ do
  foldM_ f 0 $ let
    li' = cycle li
    in take n $ zip li' (tail li')
  where
    kf a b = keyframe a b
    opacity _ = pure ()
    f prc (url, next) = do
      kf prc $ do
        background url
        backgroundSize "cover"
        opacity 1
      kf (prc + imgP) $ do
        background url
        backgroundSize "cover"
        opacity 1
      -- kf (prc + imgP + fadeHalfP) $ do
      --   background url
      --   backgroundSize "cover"
      --   opacity 0
      -- kf (prc + imgP + fadeHalfP + 0.1) $ do
      --   background "none"
      --   backgroundSize "cover"
      --   opacity 0
      -- kf (prc + imgP + fadeHalfP + 0.11) $ do
      --   background next
      --   backgroundSize "cover"
      --   opacity 0
      kf (prc + imgP + fadeHalfP + fadeHalfP) $ do
        background next
        backgroundSize "cover"
        opacity 1

      pure (prc + imgP + fadeHalfP + fadeHalfP)

    fade = ft * 2
    period = image + fade
    n = P.length li
    periodP = 100.0 P./ fromIntegral n
    imgP = periodP * (image P./ period)
    fadeHalfP = periodP * (ft P./ period)
