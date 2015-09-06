module JS
   (

   -- test, module JS_Monad, module Control.Monad.Writer, module Control.Monad.State, module Control.Monad.Reader, module Control.Monad.Identity,
   -- | JSM meta
     M, S, runM, eval, eval', {-run,-} pr, def, Text

   -- | JSM primitives
   , new -- , new'
   , block -- , block', blockExpr
   , newf , newf' , func
   , call, call0, call1, bare, arg

   , retrn
   , lit, ulit
   , ex
   , browser
   -- , untype

   -- | JS_Syntax reexports
   , Code
   , Expr(Undefined, Null, Par, Literal, ULit, Cast) -- , True, False)
   , E(..)
   , rawStm, rawExpr
   , ULiteral(..)

   -- | JS_Types reexports
   , JT.String(..), JT.Number(..), JT.Array(..), JT.Object(..), JT.Bool(..)
   , JT.Regex(..), JT.NumberI

   -- | Attribute and array index
   , (!.), (.!),  {- shorthand: -} (!-)

   -- | Operators
   , (.=)
   , (.==), (.!=), (.===), (.!==)
   , (.&&), (.||)
   , (.<), (.>), (.<=), (.>=)
   , (.+), (.-), (.*), (./)
   -- , (.+=), (.-=), (.*=), (./=)

   -- | Control flow
   , for, forin
   , ifelse, ifonly
   , ternary

   -- | Typed functions
   -- , a1, a2, a3, a4, a5 -- apply typed function to tuple-arguments
   -- , (-/)

   -- | Defined variables
   , arguments

   -- | from JS_DOM
   , module JS_DOM
   {-
   , window, location
   , on, findBy
   , zepto, onload
   , tag, createHtml
   , appendChild
   , toJson -}

   -- JS_API
   -- Array
   , module JS_API
   -- Web_HTML
   , toOn

   -- , def -- Data.Default
   )
   where

import Prelude2 hiding ((.-), for, (.=), (.>))

import Data.Default
import Common
import JS_Types as JT
import JS_Syntax
import JS_Monad
import JS_API

import JS_Ops_Untyped
import JS_DOM

import Web_HTML hiding (Progress)
