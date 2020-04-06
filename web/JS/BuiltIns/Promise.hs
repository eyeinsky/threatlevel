module JS.BuiltIns.Promise where

import X.Prelude
import JS.DSL

newPromise executor =  call1 (New $ ex "Promise") executor

reject = ex "Promise" !// "reject"
