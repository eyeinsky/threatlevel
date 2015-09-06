module IdentifierSource
   ( jsIdentifierSource
   ) where

import Prelude2
import Data.Function (on)
import Data.Ord (Ordering(..))
import Data.List (sort)

next str = let (i,ml) = initLast str
   in case ml of
      Just l -> if l == 'z'
         then next i ++ "a"
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

jsIdentifierSource = filterFrom (lengthAlphaSort reserved) $ iterate next "a"
   where
      reserved = nub $ ecma1 <> ecma2 <> ecma5 <> ecma6
      lengthAlphaSort li = sort =<<$ groupBy (on eq length) $ sortBy (on compare length) li

filterFrom xs'@ (x : xs) ys'@ (y : ys) = case mycompare x y of
   GT -> y : filterFrom xs' ys
   EQ -> filterFrom xs ys
   _ -> filterFrom xs ys'
   where
      mycompare a b = case lenCmp a b of
         GT -> GT
         EQ -> compare a b
         LT -> LT
filterFrom _ ys = ys

-- * Reserved by ECMA

-- | Source: https://mathiasbynens.be/notes/reserved-keywords

ecma1 = [ "do", "if", "in", "for", "new", "try", "var", "case", "else", "enum",
   "null", "this", "true", "void", "with", "break", "catch", "class", "const",
   "false", "super", "throw", "while", "delete", "export", "import", "return",
   "switch", "typeof", "default", "extends", "finally", "continue", "debugger",
   "function"
   ]

ecma2 = ["do", "if", "in", "for", "int", "new", "try", "var", "byte", "case",
   "char", "else", "enum", "goto", "long", "null", "this", "true", "void", "with",
   "break", "catch", "class", "const", "false", "final", "float", "short",
   "super", "throw", "while", "delete", "double", "export", "import", "native",
   "public", "return", "static", "switch", "throws", "typeof", "boolean",
   "default", "extends", "finally", "package", "private", "abstract", "continue",
   "debugger", "function", "volatile", "interface", "protected", "transient",
   "implements", "instanceof", "synchronized"
   ]

ecma5 = [ "do", "if", "in", "for", "let", "new", "try", "var", "case", "else",
   "enum", "eval", "null", "this", "true", "void", "with", "break", "catch",
   "class", "const", "false", "super", "throw", "while", "yield", "delete",
   "export", "import", "public", "return", "static", "switch", "typeof",
   "default", "extends", "finally", "package", "private", "continue", "debugger",
   "function", "arguments", "interface", "protected", "implements", "instanceof",
   "NaN", "Infinity", "undefined"
   ]

ecma6 = [ "do", "if", "in", "for", "let", "new", "try", "var", "case", "else",
   "enum", "eval", "null", "this", "true", "void", "with", "await", "break",
   "catch", "class", "const", "false", "super", "throw", "while", "yield",
   "delete", "export", "import", "public", "return", "static", "switch", "typeof",
   "default", "extends", "finally", "package", "private", "continue", "debugger",
   "function", "arguments", "interface", "protected", "implements", "instanceof"
   ]

-- * Helpers

lenCmp (a : as) (b : bs) = lenCmp as bs
lenCmp _ (b : bs) = LT
lenCmp (a : as) _ = GT
lenCmp _ _ = EQ
