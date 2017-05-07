module CSS.Example where

import Prelude2

import Web.Browser

import CSS.Internal
import CSS.Monad
import CSS.Shorthands

import HTML hiding (em)
import Render

test = do
  prop "display" "flex"
  display "level-1"
  hover $ do
    display "level-2"
    hover $ do
      display "level-3"
    descendant "strong" $ do
      fontSize $ px 12
    media "print" $ do
      color "red"
  keyframes "some-animation" $ do
    keyframe 0 $ do
      backgroundColor "red"
    keyframe 100 $ do
      backgroundColor "blue"
  media "screen" $ do
    descendant "strong" $ do
      hover $ do
        backgroundColor "pink"
      fontSize $ em 2

testRun = render $ run Chrome (TagName "body") test
