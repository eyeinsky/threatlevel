module X.Prelude
  ( module X.Prelude
  , module Export
  ) where

import Prelude as Export hiding
  ( div, rem, (/), log, const, span -- Prelude
  , (^) -- GHC.Real
  , fail
  , break
  )

import Data.Default as Export
import Data.Maybe as Export
import Data.Text.Lazy.Lens as Export (packed)
import Data.Either as Export (partitionEithers)
import Data.List as Export (isPrefixOf)
import Data.Functor as Export
import Data.Monoid as Export hiding (First(..), Last(..))
import Data.Semigroup as Export
import Data.Coerce as Export

import Control.Applicative as Export ((<|>))
import Control.Monad.IO.Class as Export
import Control.Monad.Identity as Export hiding (fail)
import Control.Monad.Reader as Export hiding (fail)
import Control.Monad.State as Export hiding (fail)
import Control.Monad.Writer as Export hiding (First(..), Last(..), fail)
import Control.Monad.RWS as Export hiding (First(..), Last(..), fail)
import Control.Monad.Fail as Export
import Control.Lens as Export hiding ((.=), (.>), transform, Setter, Getter, Context, forOf)

import GHC.Generics as Export (Generic)
import Debug.Trace as Export

import Data.Text.Multiline as Export
import Common.Lens as Export
import Common.Prelude as Export

import qualified Data.Text.Lazy as TL

bool :: p -> p -> Bool -> p
bool f t b = if b then t else f

eq :: Eq a => a -> a -> Bool
eq = (==)

notEq :: Eq a => a -> a -> Bool
notEq = (/=)

neq :: Eq a => a -> a -> Bool
neq = (/=)

todoMsg :: String -> a
todoMsg msg = trace msg (error msg)

-- Control.Monad
infixr 0 >>=$
(>>=$) :: Monad m => m a -> (a -> m b) -> m b
(>>=$) = (>>=)

infixr 0 =<<$
(=<<$) :: Monad m => (a -> m b) -> m a -> m b
(=<<$) = (=<<)

kebab2camel :: TL.Text -> TL.Text
kebab2camel t = TL.concat $ x : map capitalise xs
  where
    x : xs = TL.splitOn "-" t
    capitalise t = let (a, b) = TL.splitAt 1 t
       in TL.toUpper a <> b

split :: Eq a => [a] -> [a] -> [[a]] -- from MissingH
split _ [] = []
split delim str = let
      (firstline, remainder) = breakList (isPrefixOf delim) str
   in firstline : case remainder of
                     [] -> []
                     x -> if x == delim
                        then [] : []
                        else split delim (drop (length delim) x)

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
