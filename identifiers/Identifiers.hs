module Identifiers
   ( startFrom
   , startFrom'
   , identifierSource
   , identifiersFilter
   , next
   , Infinite(..), toInfinite
   , Increment(..)
   ) where

import Prelude
import Data.Function (on)
import Data.Ord (Ordering(..))
import Data.List (sort, sortBy, nub, groupBy)
import Control.Monad.State

import qualified Data.Text as T

import Control.Lens

-- * State helper

next :: (MonadState s m, Increment a) => Lens' s a -> m (Element a)
next lens = head' <$> (lens <<%= tail')

-- * T.Text

startFrom :: T.Text -> [T.Text]
startFrom str = if T.null str
  then "a" : go 'a' ""
  else go (T.head str) (T.tail str)
  where
    go :: Char -> T.Text -> [T.Text]
    go 'z' init = let
        init' = head $ startFrom init
        new = 'a'
      in T.cons new init' : go new init'
    go prev init = let
        new = succ prev
      in T.cons new init : go new init

identifierSource :: [T.Text]
identifierSource = startFrom ""

-- * String

startFrom' :: String -> [String]
startFrom' str = if null str
  then "a" : go 'a' ""
  else go (head str) (tail str)
  where
    go :: Char -> String -> [String]
    go 'z' init = let
        init' = head $ startFrom' init
        new = 'a'
      in (new : init') : go new init'
    go prev init = let
        new = succ prev
      in (new : init) : go new init

identifiersFilter :: [T.Text] -> [T.Text]
identifiersFilter li = filterFrom (lengthAlphaSort reserved) $ map T.reverse $ startFrom ""
  where
    lengthAlphaSort :: [T.Text] -> [T.Text]
    lengthAlphaSort li = sort =<< (groupBy (on (==) T.length) $ sortBy (on compare T.length) li)

    reserved :: [T.Text]
    reserved = nub li

    filterFrom xs'@ (x : xs) ys'@ (y : ys) = case lenCmp x y of
      GT -> y : filterFrom xs' ys
      EQ -> filterFrom xs ys
      _ -> filterFrom xs ys'
      where
        lenCmp a b = case compare (T.length a) (T.length b) of
          EQ -> compare a b
          ord -> ord
    filterFrom _ ys = ys

-- * Infinite

data Infinite a = Infinite a (Infinite a) deriving Functor

toInfinite :: [a] -> Infinite a
toInfinite xs' = case xs' of
  (x : xs) -> Infinite x (toInfinite xs)
  _ -> error "this should never happen"

-- * Increment

class Increment a where
  type Element a
  increment :: a -> (Element a, a)

instance Increment [a] where
  type Element [a] = a
  increment xs = case xs of
    x : xs -> (x, xs)
    _ -> error "Identifiers: Increment [a]: Increment for lists is partial, use Infinite instead"

instance Increment (Infinite a) where
  type Element (Infinite a) = a
  increment (Infinite x xs) = (x, xs)

head' :: Increment a => a -> Element a
head' a = x where (x, xs) = increment a

tail' :: Increment a => a -> a
tail' a = xs where (x, xs) = increment a
