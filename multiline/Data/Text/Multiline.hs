module Data.Text.Multiline
   ( multiline
   , unindent
   ) where

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Prelude

multiline :: QuasiQuoter
multiline = QuasiQuoter { quoteExp = expr, quotePat = pat, quoteType = type_, quoteDec = dec }

unindent :: QuasiQuoter
unindent = QuasiQuoter { quoteExp = (expr . go), quotePat = pat, quoteType = type_, quoteDec = dec }
  where
    go :: String -> String
    go str = let
        ls = lines $ str
        n = minimum $ map (length . takeWhile (== ' ')) $ filter (not . all (== ' ')) $ ls
        unindented = map (drop n) ls
      in intercalate "\n" $ dropWhile null unindented

expr :: String -> Q Exp
expr = litE . stringL

pat :: String -> Q Pat
pat = litP . stringL

type_ :: String -> Q Type
type_ _ = undefined

dec :: String -> Q [Dec]
dec _ = return []
