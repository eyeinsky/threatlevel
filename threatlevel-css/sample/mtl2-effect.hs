
{- | Test MTL CSS. -}

module Main where

import Common.Prelude

import CSS
import CSS.DSL.MTL2
import Render

test :: CSS m => Prop m => m ()
test = do
  -- | Top-level color, added to * ("any tag" selector)
  color "cyan"

  -- | Create new classes with content
  class_a :: Class <- css $ do
    backgroundColor "red"
    borderRadius $ px 6
  rule class_a $ do
    borderRadius $ px 7

  class_b <- css $ do
    display "flex"
    flex "row nowrap"
    color "blue"

  -- | Declaration for top-level selector, which by default is * ("any tag")
  color "blue"

  class_c <- css $ do
    content $ String "in .c"
    hover $ do
      content $ String "in .c:hover"
      child Any $ do
        content $ String "in .c:hover > *"
        margin $ em 1 <> em 1
        sibling class_b $ do
          padding $ vh 100

  atRule "media" "(max-width: 1024px)" $ do
    content $ String "In bare media"
    class_d <- css $ do
      content $ String "I'm in the media too, but in a class"
      color "dark-blue"
    return ()

rendered :: IO ()
rendered = let
  conf = Pretty 2
  in printRender conf (test :: MonoCSS ())

ast :: IO ()
ast = let (Infinite n _, (w, _)) = runFresh test
  in do
  print n
  print w

main :: IO ()
main = rendered
-- main = ast
