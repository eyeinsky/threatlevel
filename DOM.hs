module DOM
  ( module DOM.Event
  , module DOM.JS
  , on
  ) where

import Pr hiding (on)
import DOM.Event
import DOM.JS
import HTML
import JS

on :: Event a => a -> Expr a1 -> Attribute
on event js = Custom (toOn event) (render $ call1 js (ex "event"))
