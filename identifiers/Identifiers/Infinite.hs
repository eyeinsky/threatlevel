module Identifiers.Infinite where

import Prelude
import Data.List
import Data.Foldable
import Data.Function


data Infinite a = Infinite a (Infinite a)

instance Functor Infinite where
  fmap f (Infinite x xs) = Infinite (f x) (fmap f xs)

instance Foldable Infinite where
  foldMap f (Infinite x xs) = f x <> foldMap f xs

instance Applicative Infinite where
  pure f = Infinite f (pure f)
  (Infinite f _) <*> xs = fmap f xs


toInfiniteUnsafe :: [a] -> Infinite a
toInfiniteUnsafe xs' = case xs' of
  (x : xs) -> Infinite x (toInfiniteUnsafe xs)
  _ -> error "toInfiniteUnsafe: finite list given as argument"

-- * Permutations

littleEndian :: forall a. Ord a => [a] -> Infinite [a]
littleEndian alphabet = toInfiniteUnsafe list
  where
    list = flip (:) <$> [] : toList (littleEndian alphabet) <*> alphabet

bigEndian :: forall a. Ord a => [a] -> Infinite [a]
bigEndian alphabet = fmap reverse $ littleEndian alphabet

-- * Helpers

-- | Filter out members in @excluded@
orderedFilter :: forall a. (Eq a, Ord a) => [[a]] -> Infinite [a] -> Infinite [a]
orderedFilter excluded xs = filterFrom excluded' xs
  where
    excluded' :: [[a]]
    excluded' = lengthAlphaSort $ nub excluded

    filterFrom :: [[a]] -> Infinite [a] -> Infinite [a]
    filterFrom xs'@ (x : xs) ys'@ (Infinite y ys) = case lenCmp x y of
      GT -> Infinite y (filterFrom xs' ys)
      EQ -> filterFrom xs ys
      _ -> filterFrom xs ys'
      where
        lenCmp a b = case compare (length a) (length b) of
          EQ -> compare a b
          ord -> ord
    filterFrom _ ys = ys

    lengthAlphaSort :: [[a]] -> [[a]]
    lengthAlphaSort li = sort =<< (groupBy (on (==) length) $ sortBy (on compare length) li)
