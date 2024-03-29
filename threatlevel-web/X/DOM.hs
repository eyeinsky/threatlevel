module X.DOM
  ( module X.DOM
  , module DOM
  ) where

import DOM

import JS
import Server.Response
import URL

redirect :: URL -> M r ()
redirect to = DOM.location !. "href" .= lit (renderURL to)
