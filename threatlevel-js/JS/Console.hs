module JS.Console where

import Prelude hiding (log)
import qualified Data.Text as TS
import JS.BuiltIns


data Console

console :: Expr Console
console = ex "console"

logStart :: JS m => Expr b -> m ()
logStart label = bare $ console !// "group" $ label

logEnd :: JS m => Expr b -> m ()
logEnd label = bare $ console !// "groupEnd" $ label

-- ** Consoleobject/debugging

log :: JS m => Expr a -> m ()
log = consoleMethod1 "log"

log2 :: JS m => Expr String -> Expr a -> m ()
log2 = consoleMethod2 "log"

consoleError :: JS m => Expr a -> m ()
consoleError = consoleMethod1 "error"

consoleError2 :: JS m => Expr String -> Expr a -> m ()
consoleError2 = consoleMethod2 "error"

dir :: JS m => Expr a -> m ()
dir = consoleMethod1 "dir"

debug_ :: JS m => TS.Text -> Expr a -> m ()
debug_ var expr = do
   ex var .= expr
   log $ toString expr + lit " (in: \"" + lit var + lit "\")"

-- * Helpers

consoleMethod :: JS m => TS.Text -> [Expr a] -> m ()
consoleMethod method args = bare $ call (console !. method) args

consoleMethod1 :: JS m => TS.Text -> Expr a -> m ()
consoleMethod1 method arg = consoleMethod method [arg]

consoleMethod2 :: JS m => TS.Text -> Expr String -> Expr a -> m ()
consoleMethod2 method label arg = bare $ call (console !. method) [label <> ":", Cast arg]
