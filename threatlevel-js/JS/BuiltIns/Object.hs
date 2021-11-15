module JS.BuiltIns.Object where

import Prelude
import JS.DSL


setAttr :: Expr a1 -> Expr b -> Expr a2 -> Expr c
setAttr k v o = call (ex "Object" !. "assign") [ex "{}", Cast o, o1]
  where o1 = pair k v

pair :: Expr a -> Expr b -> Expr c
pair k v = Lit $ Object [(Right $ Cast k, Cast v)]
