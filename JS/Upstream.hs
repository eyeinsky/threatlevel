module JS.Upstream
  ( module JS.Upstream
  , module JS
  ) where

import JS


a !/ b = call0 (a !. b)
a !// b = call1 (a !. b)
