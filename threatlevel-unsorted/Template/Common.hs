module Template.Common where

import Common.Prelude
import HTML hiding (Id)
import JS
import DOM.JS
import DOM.Core
import Web.Apis.DOM

-- * Context

-- | Context contains the source object, the related dom nodes and,
-- possibly, the fragment
data Context a
data Node

context :: Expr a -> Expr [Node] -> Expr DocumentFragment -> Expr (Context a)
context a nodes fragment = lit [Cast a, Cast nodes, Cast fragment]

source :: Expr (Context a) -> Expr a
source ctx = ctx !- 0

nodes :: Expr (Context a) -> Expr [Node]
nodes ctx = ctx !- 1

fragment :: Expr (Context a) -> Expr DocumentFragment
fragment ctx = ctx !- 2

-- * Helpers

-- | Iterate through the nodes array, run action
withNodes :: JS m => Expr (Context a) -> (Expr Node -> m b) -> m ()
withNodes ctx go = iterArray (nodes ctx) $ \ix -> do
  node <- const $ nodes ctx !- ix -- <- remember me
  go node

fragment2nodes :: Expr DocumentFragment -> Expr [Node]
fragment2nodes fragment = ex "Array" !// "from" $ fragment !. "childNodes"

-- ** Context

createContext :: JS m => Expr a -> Html -> m (Expr (Context a))
createContext item html = do
  fragment <- createHtmls html
  ctx <- const $ context item (fragment2nodes fragment) fragment
  return ctx

-- | Append DOM nodes from @context@ to element with @id@
appendContext :: JS m => Expr (Context a) -> Id -> m ()
appendContext context id = do
  dest <- const $ querySelector id document
  withNodes context $ \node -> do
    bare $ dest !// "appendChild" $ node

-- | Find first element matching @selector@ from @context@
libQuerySelectorNodes :: JS m => JSSelector s => s -> Expr [Node] -> m (Expr Tag)
libQuerySelectorNodes selector nodes' = do
  ret <- let_ Null
  iterArray nodes' $ \ix -> do
    res <- const $ querySelector' selector (nodes' !- ix)
    ifonly (res .!== Null) $ ret .= res
  return ret

-- mkGet :: JS m => Class -> m (Expr [Node] -> Expr Tag)
mkGet cls = fn $ \ns ->
  return_ =<< libQuerySelectorNodes cls ns
