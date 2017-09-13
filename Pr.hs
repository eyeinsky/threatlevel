module Pr
  ( module Prelude2
  , module Prelude2.Has
  , module Text.Exts
  , module Data.Default
  , module Data.ByteString.Lens
  , module Data.Text.Lazy.Lens
  , module Control.Monad
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.Writer
  , module Control.Monad.State
  , module Control.Monad.RWS
  , module Pr
  ) where

import Prelude2 hiding (un, text, Text)
import Prelude2.Has hiding (id)
import Data.Default
import Data.ByteString.Lens
import Data.Text.Lazy.Lens hiding (Text, _Text, packed, builder, text, unpacked)
import Text.Exts (kebab2camel)

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader hiding (Reader)
import Control.Monad.Writer hiding (Writer)
import Control.Monad.State  hiding (State)
import Control.Monad.RWS (RWST(..), runRWST)

class HasClasses s a | s -> a where
  classes :: Lens' s a
  {-# MINIMAL classes #-}
