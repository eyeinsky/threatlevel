module Common.Prelude
  ( module Common.Prelude
  , module Export
  ) where

import Prelude as Export
  hiding ((^), (/), const, rem, div, log, span)
import Data.Maybe as Export
import Data.String as Export
import Control.Monad as Export
import Data.Functor as Export
import Data.Foldable as Export
import Control.Lens as Export hiding
  ((.=), (.>), Empty, Setter, Getter, Const, Context, transform, assign)
import Control.Arrow as Export hiding (left, right)

import Data.Proxy as Export
import Data.Void as Export
import Data.List.Fixed as Export
import Data.Default as Export
import Data.Kind as Export
import Data.Coerce as Export
import Identifiers as Export hiding (Element)

import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Char
import Data.List

import Debug.Trace as Export


infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

todo :: a
todo = undefined

todoMsg :: String -> a
todoMsg msg = trace msg (error msg)

-- Data.Bool
infix 1 ?
(?) :: Bool -> p -> p -> p
(?) bool = if bool then (\a _ -> a) else (\_ a -> a)

bool :: a -> a -> Bool -> a
bool f t b = if b then t else f

-- Control.Monad
(>>=$) :: Monad m => m a -> (a -> m b) -> m b
infixr 0 >>=$
(>>=$) = (>>=)

(=<<$) :: Monad m => (a -> m b) -> m a -> m b
infixr 0 =<<$
(=<<$) = (=<<)

-- * String, Text, Text.Lazy

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

-- * Data.Text

tlKebab2camel :: TL.Text -> TL.Text
tlKebab2camel t = TL.concat $ x : map capitalise xs
  where
    x : xs = TL.splitOn "-" t
    capitalise t = let (a, b) = TL.splitAt 1 t
       in TL.toUpper a <> b

-- | Convert strict text kebab-case to camelCase
tsKebab2camel :: TS.Text -> TS.Text
tsKebab2camel k = TL.toStrict $ tlKebab2camel $ TL.fromStrict k

-- * Data.List

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
