module JS.BuiltIns.Promise where

import Common.Prelude
import JS.DSL

newPromise :: Expr b -> Expr c
newPromise executor =  call1 (New $ ex "Promise") executor

reject :: Expr b -> Expr c
reject = ex "Promise" !// "reject"

-- * Util

-- | Create a promise and return it together with the functions to
-- resolve and reject it.
mkPromise :: JS m => m (Expr resolve, Expr reject, Expr p)
mkPromise = do
  resolve <- let_ Null
  reject <- let_ Null
  executor <- newf $ \resolve' reject' -> do
    resolve .= resolve'
    reject .= reject'
  promise <- const $ newPromise executor
  return (resolve, reject, promise)
