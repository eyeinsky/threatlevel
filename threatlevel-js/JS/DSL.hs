module JS.DSL
  ( module Export

  -- * JS.Syntax
  , JS.Syntax.Conf(..)
  , Statement(BareExpr, TryCatchFinally)
  , Expr(Undefined, Null, Par, Lit, Cast, AnonFunc, Raw, In, New, Await, Assign)
  , Attr(..)
  , Literal(..)
  , Code
  , call, call0, call1, (!.), (.!), ex
  ) where

import JS.DSL.Syntax as Export
-- import JS.DSL.MTL as Export
import JS.DSL.Polysemy as Export
import JS.Syntax
