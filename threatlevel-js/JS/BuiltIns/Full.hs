module JS.BuiltIns.Full
  ( module JS.DSL
  , module JS.Derive

  , module JS.Apis.EventLoop
  , module JS.BuiltIns.JSON
  , module JS.BuiltIns.Date
  , module JS.BuiltIns.String
  , module JS.BuiltIns.Number
  , module JS.BuiltIns.Array
  , module JS.BuiltIns.Object
  , module JS.BuiltIns.Promise
  , module JS.BuiltIns.Full
  ) where

import JS.DSL
import JS.Derive

import JS.Apis.EventLoop

import JS.BuiltIns.JSON
import JS.BuiltIns.Date
import JS.BuiltIns.String
import JS.BuiltIns.Number
import JS.BuiltIns.Array
import JS.BuiltIns.Object
import JS.BuiltIns.Promise

-- | Blob, JSON
data Blob
jsonBlob :: Expr a -> Expr Blob
jsonBlob obj = call (New (ex "Blob")) [arr, meta]
  where
    arr = lit [stringify obj]
    meta = lit [("type", "application/json")]
