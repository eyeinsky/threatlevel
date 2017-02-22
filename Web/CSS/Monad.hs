module Web.CSS.Monad where

import Prelude2
import qualified Data.Text.Lazy as TL

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import Web.Browser
import Web.CSS.Internal


-- * For export

type M = CSSM

run :: SelectorFrom a => Browser -> a -> CSSM () -> [Rule]
run b s m = runCSSM (selFrom s, b) m

rule :: SelectorFrom a => a -> DM () -> CSSM ()
rule s ds = tell $ CSSW ([mkRule (selFrom s) (runIdentity . execWriterT $ ds)], [])

prop :: TL.Text -> Value -> CSSM ()
prop k v = tell $ CSSW ([], [mkDeclaration k v]) :: CSSM ()

-- * DSL setup

type R = (Selector, Browser)
type CSSM = WriterT CSSW (ReaderT R Identity)
newtype CSSW = CSSW ([Rule], [Declaration])

runCSSM :: R -> CSSM () -> [Rule]
runCSSM r@ (s, _) m = r' : rs
  where
    x@ (_, CSSW (rs, ds)) = runIdentity . flip runReaderT r . runWriterT $ m
    r' = mkRule s ds

instance Monoid CSSW where
  mempty = CSSW ([], [])
  mappend (CSSW (a, b)) (CSSW (a', b')) = CSSW (a <> a', b <> b')

apply :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
apply f s = go s
  where
    go :: Selector -> Selector
    go s = case s of
      Simple ss -> Simple (f ss)
      Combined op s ss -> Combined op s (f ss)

pseudo' :: TL.Text -> SimpleSelector -> SimpleSelector
pseudo' t s = s & pseudos %~ (p:)
  where p = Pseudo t

pseudo :: TL.Text -> CSSM () -> CSSM ()
pseudo t m = do
  (s, b) <- ask
  let hoovered = apply (pseudo' t) s
      rs = runCSSM (hoovered, b) m
  tell $ CSSW (rs, [])

combinator :: SOp -> SimpleSelector -> CSSM () -> CSSM ()
combinator c d m = do
  (s, b) <- ask
  tell $ CSSW (runCSSM (Combined c s d, b) m, [])
