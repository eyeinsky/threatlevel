module Common.Prelude
  (
  -- * Helpers
    module Common.Prelude
  -- * Re-exports
  , module Common.BasePreludeReExports
  ) where

import Common.BasePreludeReExports

import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Char
import Data.List
import Debug.Trace

-- | Flipped @(.)@
infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

-- | Placeholder that is easy to grep. Equals @undefined@.
todo :: a
todo = undefined

-- | Same as above but fails with a message.
todoMsg :: String -> a
todoMsg msg = trace msg (error msg)

-- | Ternary operator, e.g @bool ? true $ false@.
infix 1 ?
(?) :: Bool -> p -> p -> p
(?) bool = if bool then (\a _ -> a) else (\_ a -> a)

-- | @if@ which takes t'Bool' as last argument.
bool :: a -> a -> Bool -> a
bool f t b = if b then t else f

-- * Control.Monad

-- | @>>=@ with @$@ fixity.
(>>=$) :: Monad m => m a -> (a -> m b) -> m b
infixr 0 >>=$
(>>=$) = (>>=)

-- | @=<<@ with @$@ fixity.
(=<<$) :: Monad m => (a -> m b) -> m a -> m b
infixr 0 =<<$
(=<<$) = (=<<)

-- * String and Text

-- | Convert kebab-case to camelCase
kebab2camel :: String -> String
kebab2camel xs = case xs of
  '-' : x' : xs'
    | isAlpha x' -> toUpper x' : kebab2camel xs'
    | otherwise -> err
  '-' : [] -> err
  x' : xs' -> x' : kebab2camel xs'
  _ -> []
  where
    err :: a
    err = error "Common.Prelude.kebab2camel: This shouldn't ever happen"

-- | Convert lazy t'Text' from kebab-case to camelCase
tlKebab2camel :: TL.Text -> TL.Text
tlKebab2camel t = case TL.splitOn "-" t of
  x : xs -> TL.concat $ x : map capitalise xs
  [] -> ""
  where
    capitalise t = let (a, b) = TL.splitAt 1 t
       in TL.toUpper a <> b

-- | Convert strict t'Text' from kebab-case to camelCase
tsKebab2camel :: TS.Text -> TS.Text
tsKebab2camel k = TL.toStrict $ tlKebab2camel $ TL.fromStrict k

-- * Data.List

-- | Split list by delimiter
split :: Eq a => [a] -> [a] -> [[a]] -- from MissingH
split _ [] = []
split delim str = let
      (firstline, remainder) = breakList (isPrefixOf delim) str
   in firstline : case remainder of
                     [] -> []
                     x -> if x == delim
                        then [] : []
                        else split delim (drop (length delim) x)

  where
    {- | Similar to Data.List.break, but performs the test on the entire remaining
    list instead of just one element. -}
    breakList :: ([a] -> Bool) -> [a] -> ([a], [a]) -- from MissingH
    breakList func = spanList (not . func)

    {- | Similar to Data.List.span, but performs the test on the entire remaining
    list instead of just one element.

    @spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@
    -}
    spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

    spanList _ [] = ([],[])
    spanList func list@(x:xs) =
        if func list
           then (x:ys,zs)
           else ([],list)
        where (ys,zs) = spanList func xs
