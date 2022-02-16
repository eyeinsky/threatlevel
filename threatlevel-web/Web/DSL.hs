module Web.DSL where

import Common.Prelude
import Data.Tuple
import Control.Monad.RWS

import qualified CSS.DSL.MTL.Mono as CSS
import qualified JS.DSL.MTL.Mono as JS
import qualified JS.Syntax as JS

{- * Boilerplate

Merge reader/state/writer of MonoJS and MonoCSS by adding apropriate
MTL actions for both.

-}

type Reader_ = (CSS.Reader_, JS.Reader_)
type State_ = (CSS.State_, JS.State_)
type Writer_ = (CSS.Writer_, JS.Writer_)

askCSS = asks fst
askJS = asks snd
localCSS f = local $ \(c, j) -> (f c, j)
localJS f = local $ \(c, j) -> (c, f j)
putCSS c' = modify $ \(c, j) -> (c', j)
putJS j' = modify $ \(c, j) -> (c, j')
getCSS = gets fst
getJS = gets snd
tellCSS t = tell (t, mempty)
tellJS t = tell (mempty, t)

-- * Mono

type Web' m = RWST Reader_ Writer_ State_ m
type Web = Web' Identity

runM :: Web' m a -> Reader_ -> State_ -> m (a, State_, Writer_)
runM m r s = runRWST m r s

run :: Web a -> Reader_ -> State_ -> (a, State_, Writer_)
run m r s = runM m r s & runIdentity

execSubBase' ask get put tell pick m = do
  (a, s, w) <- run m <$> ask <*> get
  let (toReturn, toTell) = pick w
  tell toTell *> put s $> (a, toReturn)

instance JS.JS Web where
  stm s = tellJS (pure s)
  freshName = JS.freshNameBase getJS putJS
  bind = JS.bindBase
  execSub m = execSubBase' ask get put tell pick m
    where pick (c, j) = (j, (c, mempty))

  -- f1 :: forall f . (C1 f, MonadFor f ~ m) => f -> m (Expr ())
  f1 = error "Web.DSL: f1 not defined"
  f2 = JS.f2Base JS.bind
  f3 = error "Web.DSL: f3 not defined"
  -- f3 :: forall f . (C3 f m) => f -> m RetUntyped

instance CSS.CSS Web where
  css m = CSS.cssBase getCSS putCSS m
  rule slike m = CSS.ruleBase localCSS tellCSS slike m
  combine f m = CSS.combineBase askCSS f m
  atRule atIdent atCond m = CSS.atRuleBase askCSS tellCSS atIdent atCond m
  execSub m = execSubBase' ask get put tell undefined m
    where pick (c, j) = (c, (mempty, j))

instance CSS.Prop Web where
  prop property value = CSS.propBase tellCSS property value
