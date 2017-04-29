module Web.CSS.Example where

import Prelude2

import Web.Browser

import Web.CSS.Internal
import Web.CSS.Monad
import Web.CSS.Shorthands

import Web.HTML
import Render

test = do
  prop "display" "flex"
  display "level-1"
  hover $ do
    display "level-2"
    hover $ do
      display "level-3"
    descendant "strong" $ do
      fontSize "jee"
  keyframes "bla" $ do
    keyframe 0 $ do
      backgroundColor "red"
    keyframe 100 $ do
      backgroundColor "blue"

testRun = render $ run Chrome (TagName "body") test
