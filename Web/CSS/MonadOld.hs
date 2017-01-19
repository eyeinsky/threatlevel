module Web.CSS.MonadOld
  ( run, prop, rule, toRules, addPseudo
  , M
  ) where

import Prelude2
import qualified Data.Text.Lazy as TL

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import Web.HTML.Core

import Web.CSS.Internal

-- * For export

type M = DM

run :: SelectorFrom a => a -> DM () -> [Rule]
run s m = toRules $ rule (selFrom s) m

toRules :: RM a -> [Rule]
toRules = snd . runRM

rule :: SelectorFrom a => a -> DM () -> RM ()
rule s ds = tell $ [ mkRule (selFrom s) (runIdentity . execWriterT $ ds) ]

prop :: TL.Text -> Value -> DM ()
prop p v = tell [mkDeclaration p v]

addPseudo :: SelectorFrom a => TL.Text -> a -> Selector
addPseudo p a = modifySelector f (selFrom a)
  where
    f (SimpleSelector mt mi cs ps) = SimpleSelector mt mi cs (nub $ Pseudo p : ps)

-- * Monad

type RM = WriterT [Rule] Identity
runRM :: RM a -> (a, [Rule])
runRM = runIdentity . runWriterT

type DM = WriterT [Declaration] Identity
runDM :: DM a -> (a, [Declaration])
runDM = runIdentity . runWriterT

execDM :: DM a -> [Declaration]
execDM = snd . runDM

ruleMToText :: RM () -> TL.Text
ruleMToText = TL.unlines . map pr . snd . runRM

(-#) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -# str = SimpleSelector mt (Just (Id str)) cs ps

(-.) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -. str = SimpleSelector mt is (Class str : cs) ps

(-:) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -: str = SimpleSelector mt is cs (Pseudo str : ps)

modifySelector :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
modifySelector f (Simple ss) = Simple (f ss)
