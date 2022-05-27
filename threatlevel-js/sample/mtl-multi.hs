{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StarIsType #-}

module Main where

import Prelude
import Data.Kind
import Control.Monad.Reader qualified as MTL
import Control.Monad.Writer qualified as MTL
import Control.Monad.Identity qualified as MTL

-- | Effect signifiers
type Signifier = Type -> Type
data Reader :: Signifier
data Writer :: Signifier


type Effect = (Type -> Type) -> Type -> Type

type family To (s :: Type) :: Effect
type instance To (Reader a) = MTL.ReaderT a
type instance To (Writer a) = MTL.WriterT a


-- '[Reader ()] a => ReaderT () Identity a
-- '[Reader (), Reader Int] a => ReaderT ((), Int) Identity a

type Transformer = (Type -> Type) -> Type -> Type

type MTL :: [Type] -> Transformer -> Type -> Type
type family MTL effs tr where
  MTL (eff : effs) tr = (To eff) (MTL effs tr)
  MTL '[] tr = tr MTL.Identity


-- type

-- type MTL' es

-- MTL.Identity :: Type -> Type
-- MTL.IdentityT :: (k -> Type) -> k -> Type

{-
- [X]
  :set -XDataKinds
  :kind! MTL '[Reader Int] = MTL.ReaderT Int MTL.Identity

- [ ]
  :kind! MTL '[Reader Int, Reader String] = MTL.ReaderT (String, (Int, ())) MTL.Identity


-}


type family Has

main :: IO ()
main = putStrLn "Hello, Haskell!"
