module JS.BuiltIns.Promise where

import X.Prelude
import JS.DSL

newPromise executor =  call1 (New $ ex "Promise") executor

reject = ex "Promise" !// "reject"

-- * Util

-- | Create a promise and return it together with the function to
-- resolve it.
mkPromise :: M r (Expr f, Expr p)
mkPromise = do
  resolve <- let_ Null
  executor <- newf $ \resolve' -> resolve .= resolve'
  promise <- const $ newPromise executor
  return (resolve, promise)
