module X.Widgets where

import Common.Prelude as P
import qualified CSS
import JS qualified
import Web

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
  _ <- rule (ssFrom elemId) $ do --  & classes <>~ [static $ coerce cls] todo
    position "fixed"
    CSS.top 0
    zIndex 1
    extraCss

  onEvent Load window $ do
    doc <- const scrollContainer
    menuElem <- const $ querySelector (jsIdName elemId) document
    initialTop <- const $ menuElem !. "offsetTop"
    is <- const $ lit False
    f <- newf $ do
      let top = doc !. "scrollTop"
          should = top .> initialTop
      ifelse should
        (ifonly (JS.not is) $ do
            addClass (jsClassName cls) menuElem
            is .= lit True
        )
        (ifonly is $ do
            addClass (jsClassName cls) menuElem
            is .= lit False
        )
    bare $ addEventListener (Cast window) Scroll f

-- | Fading background images

-- faidingImages li image ft = keyframes $ do
--   foldM_ f 0 $ let
--     li' = cycle li
--     in take n $ zip li' (tail li')
--   where
--     kf a b = keyframe a b
--     opacity _ = pure ()
--     f prc (url, next) = do
--       kf prc $ do
--         background url
--         backgroundSize "cover"
--         opacity 1
--       kf (prc + imgP) $ do
--         background url
--         backgroundSize "cover"
--         opacity 1
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
    --   kf (prc + imgP + fadeHalfP + fadeHalfP) $ do
    --     background next
    --     backgroundSize "cover"
    --     opacity 1

    --   pure (prc + imgP + fadeHalfP + fadeHalfP)

    -- fade = ft * 2
    -- period = image + fade
    -- n = P.length li
    -- periodP = 100.0 P./ fromIntegral n
    -- imgP = periodP * (image P./ period)
    -- fadeHalfP = periodP * (ft P./ period)
