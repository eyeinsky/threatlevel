module X.DOM
  ( module X.DOM
  , module DOM
  ) where

import DOM

import JS
import Web.Response

redirect to = DOM.location !. "href" .= ulit (renderURL to)
