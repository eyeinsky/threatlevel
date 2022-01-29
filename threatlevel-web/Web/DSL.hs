module Web.DSL where

import Common.Prelude

import Polysemy hiding (run)
import Polysemy.Internal qualified as Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer
import Polysemy.Fixpoint


import CSS
import CSS.DSL.Polysemy.Base qualified as CSS

import JS.DSL.Polysemy qualified as JS
import JS.DSL.Polysemy.Base qualified as JS.Base





type Base = Polysemy.Append JS.Base.Base CSS.Base

type T1 :: EffectRow -> EffectRow
type T1 rest = Prop : rest

type T2 :: EffectRow -> EffectRow
type T2 rest = CSS (T1 rest) : T1 rest

type T3 :: EffectRow -> EffectRow
type T3 rest = JS.JS (T2 rest) : T2 rest


-- JS: Syntax.Conf -> Lib -> Used -> Idents -> a

webToBase :: Members Base r => Sem (T3 r) a -> Sem r a
webToBase m = m
  & JS.jsToBase undefined -- (\_ _ _ _ m -> webToBase m)
  & CSS.cssToBase undefined
  & CSS.propToBase
--   & JS.runBase undefined undefined undefined undefined
 -- & runWriter
 --  & runState undefined
 --  & runState undefined
 --  & runState undefined
 --  & runReader undefined
 --  & runWriter undefined
