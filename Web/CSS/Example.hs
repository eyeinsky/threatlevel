module Web.CSS.Example where

import Web.CSS

test :: RM ()
test = let
    e = SimpleSelector Nothing Nothing [] []
  in do
  rule (e -# "id" -. "c1" -. "c2" -: "p1" -: "p2") $ do
      prop "jee" $ hex 5
      prop "background-color" $ hex 7
