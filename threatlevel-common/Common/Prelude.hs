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


infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

todo :: a
todo = undefined

-- Data.Bool
infix 1 ?
(?) :: Bool -> p -> p -> p
(?) bool = if bool then (\a _ -> a) else (\_ a -> a)

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



tlKebab2camel :: TL.Text -> TL.Text
tlKebab2camel t = TL.concat $ x : map capitalise xs
  where
    x : xs = TL.splitOn "-" t
    capitalise t = let (a, b) = TL.splitAt 1 t
       in TL.toUpper a <> b

-- | Convert strict text kebab-case to camelCase
tsKebab2camel :: TS.Text -> TS.Text
tsKebab2camel k = TL.toStrict $ tlKebab2camel $ TL.fromStrict k
