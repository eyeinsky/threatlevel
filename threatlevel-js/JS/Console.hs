module JS.Console where

import Prelude hiding (log)
import qualified Data.Text as TS
import JS.BuiltIns


data Console

console :: Expr Console
console = ex "console"

logStart :: Expr b -> M r ()
logStart label = bare $ console !// "group" $ label

logEnd :: Expr b -> M r ()
logEnd label = bare $ console !// "groupEnd" $ label

-- ** Consoleobject/debugging

log :: Expr a -> M r ()
log = consoleMethod1 "log"

log2 :: Expr String -> Expr a -> M r ()
log2 = consoleMethod2 "log"

consoleError :: Expr a -> M r ()
consoleError = consoleMethod1 "error"

consoleError2 :: Expr String -> Expr a -> M r ()
consoleError2 = consoleMethod2 "error"

dir :: Expr a -> M r ()
dir = consoleMethod1 "dir"

debug_ :: TS.Text -> Expr a -> M r ()
debug_ var expr = do
   ex var .= expr
   log $ toString expr + lit " (in: \"" + lit var + lit "\")"

-- * Helpers

consoleMethod :: TS.Text -> [Expr a] -> M r ()
consoleMethod method args = bare $ call (console !. method) args

consoleMethod1 :: TS.Text -> Expr a -> M r ()
consoleMethod1 method arg = consoleMethod method [arg]

consoleMethod2 :: TS.Text -> Expr String -> Expr a -> M r ()
consoleMethod2 method label arg = bare $ call (console !. method) [label <> ":", Cast arg]
