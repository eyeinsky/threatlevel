module JS.Console where

import Prelude
import JS.BuiltIns


data Console

console :: Expr Console
console = ex "console"

logStart label = bare $ console !// "group" $ label
logEnd label = bare $ console !// "groupEnd" $ label

-- ** Consoleobject/debugging

consoleLog :: [Expr a] -> M r ()
consoleLog args = bare $ call (console !. "log") args

consoleError :: [Expr a] -> M r ()
consoleError args = bare $ call (console !. "error") args

log msg = consoleLog [msg]
dir msg = bare $ call1 (console !. "dir") msg

debug var expr = do
   ex var .= expr
   consoleLog [toString expr + lit " (in: \"" + lit var + lit "\")"]

log2 label obj = consoleLog [Cast label, Cast obj]
