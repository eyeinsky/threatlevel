
{- | Test polysemy CSS without interpretation & print. -}

module Main where

import Common.Prelude
import Common.Polysemy

import CSS.DSL.Polysemy.Effect
import CSS

test :: Member CSS r => Member Prop r => Sem r ()
test = do

  -- | Create new classes with content
  class_a :: Class <- css $ do
    backgroundColor "red"
    borderRadius $ px 6

  class_b <- css $ do
    display "flex"
    flex "row nowrap"
    color "blue"

  -- | Declaration for top-level selector, which by default is * ("any tag")
  color "blue"

  class_c <- css $ do
    hover $ do
      child Any $ do
        margin $ em 1 <> em 1
        sibling class_b $ do
          padding $ vh 100

  atRule "media" "(max-width: 1024px)" $ do
    class_d <- css $ do
      content $ String "I'm in the media too, but in a class"
      color "dark-blue"
    return ()

main :: IO ()
main = pure ()
