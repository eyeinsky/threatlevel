module WW where

test = do
   
   i <- elem
   o <- elem

   render $ onKey i ~> append o
