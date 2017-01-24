module Web.CSS.Example where

import Prelude2

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

testRun = render $ run (TagName "body") test
