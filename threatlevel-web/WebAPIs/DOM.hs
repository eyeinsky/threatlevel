module WebAPIs.DOM where

import Common.Prelude
import JS
import DOM

-- * Move @element@ content to document fragment

contentToFragment :: Expr Tag -> M r (Expr DocumentFragment)
contentToFragment element = do
  fragment :: Expr DocumentFragment <- const mempty
  -- | WORKS: Always get childNodes[0]
  cn <- const $ element !. "childNodes"
  let first = cn !- 0
  while first $ bare $ fragment !// "appendChild" $ first
  return fragment

contentToFragment2 :: Expr Tag -> M r (Expr DocumentFragment)
contentToFragment2 element = do
  fragment :: Expr DocumentFragment <- const mempty
  -- | WORKS: Always get .firstChild
  let firstChild = element !. "firstChild"
  while firstChild $
    bare $ fragment !// "appendChild" $ this !// "removeChild" $ firstChild
  return fragment

-- | DOESN'T WORK: for of doest't work, as the element count changes
-- X.forOf arr $ \e -> do
--   bare $ new !// "appendChild" $ e
