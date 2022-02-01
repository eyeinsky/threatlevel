module Main where

import Common.Prelude
import qualified Data.Text.Lazy.IO as TL
import CSS.Syntax

import CSS
import CSS.DSL.Polysemy.Effect
import CSS.DSL.Polysemy.Base

import Polysemy
import Render

-- * CSS.DSL.Polysemy

testCss
  :: forall s r
   . ( Member Prop r
     , r ~ (CSS s : s)
     )
  => Sem r ()
testCss = do

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

{- | Result:

    .a {
        background-color: red;
    }
    .a {
        border-radius: 6px;
    }
    .b {
        display: flex;
    }
    .b {
        flex: row nowrap;
    }
    .b {
        color: blue;
    }
    * {
        color: blue;
    }
    .c:hover > * {
        margin: 1.0em 1.0em;
    }
    .c:hover > * + .b {
        padding: 100.0vh;
    }
    @media (max-width: 1024px) {
        .d {
            content: "I'm in the media too, but in a class";
        }
        .d {
            color: dark-blue;
        }
    }

-}

main :: IO ()
main = let
  renderConf :: Render.Conf (MonoCSS' ())
  renderConf = Pretty 0
  -- renderConf = Minify
  in TL.putStrLn $ render @(MonoCSS' ()) renderConf testCss
