module Lexic where

import Prelude2

nextLexic str = let (i,ml) = initLast str
   in case ml of
      Just l -> if l == 'z'
         then nextLexic i ++ "a"
         else i ++ [succ l]
      _ -> "a"
   where
      az = ['a'..'z']

      initLast xs = case xs of
         (x:xs) -> let (init,last) = initLast xs
            in case last of
               Nothing -> (init, Just x)
               Just z -> (x:init, Just z)
         _ -> ([], Nothing)    

alphabetSrc = iterate nextLexic "a"

{- 

   a
   ..
   z
   aa
   ..
   az
   aaa
   .. ..

-}


