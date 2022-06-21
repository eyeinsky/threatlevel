module Web.Apis.DOM
  ( module Web.Apis.DOM
  , module Web.Apis.Event
  ) where

import Common.Prelude
import Data.Text qualified as TS
import CSS qualified
import JS
import HTML
import DOM.Core qualified as DOM

import Web.Apis.Event

data Window
data Location

window :: Expr Window
window = ex "window"

location :: Expr Location
location = window !. "location"

-- * Move @element@ content to document fragment

contentToFragment :: JS m => Expr Tag -> m (Expr DocumentFragment)
contentToFragment element = do
  fragment :: Expr DocumentFragment <- const mempty
  -- | WORKS: Always get childNodes[0]
  cn <- const $ element !. "childNodes"
  let first = cn !- 0
  while first $ bare $ fragment !// "appendChild" $ first
  return fragment

contentToFragment2 :: JS m => Expr Tag -> m (Expr DocumentFragment)
contentToFragment2 element = do
  fragment :: Expr DocumentFragment <- const mempty
  -- | WORKS: Always get .firstChild
  let firstChild = element !. "firstChild"
  while firstChild $
    bare $ fragment !// "appendChild" $ this !// "removeChild" $ firstChild
  return fragment

-- | DOESN'T WORK: for of doest't work, as the element count changes
-- X.forOf arr $ \e -> do
--   bare $ new !// "appendChild" $ e

-- * Finding elements

class JSSelector a where
  jsSelectorFrom :: a -> Expr String
instance JSSelector (Expr String) where
  jsSelectorFrom a = a
instance {-# OVERLAPPABLE #-} CSS.SelectorFrom a => JSSelector a where
  jsSelectorFrom s = lit $ render CSS.Minify $ CSS.selFrom s

matches :: JSSelector a => a -> Expr e -> Expr Bool
matches s e = call1 (e !. "matches") (jsSelectorFrom s)

querySelector :: JSSelector a => a -> Expr e -> Expr Tag
querySelector s e = call1 (e !. "querySelector") (jsSelectorFrom s)

-- | Query selector which includes the root node
querySelector' :: JSSelector a => a -> Expr e -> Expr Tag
querySelector' selector root = (matches selector root .&& root) .|| querySelector selector root

querySelectorAll :: JSSelector a => a -> Expr e -> Expr [Tag]
querySelectorAll s e = call1 (e !. "querySelectorAll") (jsSelectorFrom s)

closest :: JSSelector s => s -> Expr Tag -> Expr Tag
closest s e = call1 (e !. "closest") (jsSelectorFrom s)

-- | Get closest parent with data-* attribute. Partial
closestData :: TS.Text -> Expr Tag -> Expr a
closestData attr el = (el !// "closest" $ lit ("[data-" <> attr <> "]")) !. "dataset" !. attr

-- * Modify

appendChild :: Expr Tag -> Expr Tag -> Expr ()
appendChild a t = call1 (t !. "appendChild") a

createDocumentFragment :: Expr DocumentFragment
createDocumentFragment = call0 (document !. "createDocumentFragment")

insertBefore :: Expr Tag -> Expr Tag -> Expr c
insertBefore a b = call (parentNode b !. "insertBefore") [a, b]

replaceChild :: Expr Tag -> Expr Tag -> Expr c
replaceChild old new = call (parentNode old !. "replaceChild") [new, old]

removeChild :: Expr Tag -> Expr Tag -> Expr Tag
removeChild parent child = call1 (parent !. "removeChild") child

parentNode :: Expr Tag -> Expr Tag
parentNode e = e !. "parentNode"

createElement :: TagName -> Expr Tag
createElement tn = document !// "createElement" $ lit $ coerce @_ @Value tn

createTextNode :: Expr String -> Expr Tag
createTextNode txt = document !// "createTextNode" $ txt

documentWrite :: Expr b -> Expr c
documentWrite what = call1 (document !. "write") what

clearContent :: Expr Tag -> Expr ()
clearContent element = Assign (element !. "innerHTML") ""

-- | Replace content of the @element@ with @fragment@. Done in such a
-- way to be an expression.
replaceContent :: Expr DocumentFragment -> Expr Tag -> Expr ()
replaceContent fragment element = Par (clearContent element) .|| (element !// "append" $ fragment)

-- * Attributes

addClass :: JS m => DOM.Class -> Expr a -> m ()
addClass cls el = bare $ call1 (el !. "classList" !. "add") $ mkExpr cls

removeClass :: JS m => DOM.Class -> Expr a -> m ()
removeClass cls el = bare $ call1 (el !. "classList" !. "remove") $ mkExpr cls

mkExpr :: DOM.Class -> Expr a
mkExpr = Cast . lit . static . coerce @_ @DOM.Value

-- * Events

preventDefault :: Event e => Expr e -> Expr ()
preventDefault e = call0 (e !. "preventDefault")

mkEventListener :: Event e => TS.Text -> Expr Tag -> e -> [Expr b] -> Expr c
mkEventListener a el et li = call (el !. a) (etStr : li)
  where etStr = lit $ eventString et

addEventListener :: Event e => Expr Tag -> e -> Expr b -> Expr c
addEventListener el et handler = mkEventListener "addEventListener" el et [handler]

removeEventListener :: Event e => Expr Tag -> e -> Expr b -> Expr c
removeEventListener el et handler = mkEventListener "removeEventListener" el et [handler]

onEvent
  :: (JS m, Function f m, Event e) => e -> Expr a -> f
  -> m (Expr b) -- (Expr (JS.Type h))
onEvent eventType obj handler = do
  handler' <- async handler
  bare $ addEventListener (Cast obj) eventType handler'
  return $ Cast handler'

-- | Attach an event handler on document load
attachOnLoad
  :: (JS m, Function f m, Event e)
  => e -> Expr a -> f -> m (Expr b) -- (Expr (JS.Type h))
attachOnLoad type_ element handler = do
  handler' <- async handler
  lit <- funcLet Func $ bare $ addEventListener (Cast element) type_ handler'
  bare $ addEventListener (Cast window) Load lit
  return $ Cast handler'


-- | Turn event handler to async iterator
iterEvent :: JS m => Event e => e -> Expr a -> m (Expr b)
iterEvent eventType element = do

  let
    eventType' = lit $ eventString eventType :: Expr String
    wrap done value = lit
      [("done", done), ("value" :: TS.Text, value)]

  event <- let_ Null
  resolver <- let_ Null

  handler <- newf $ \ev -> do
    ifelse resolver
      (bare $ call1 resolver $ wrap false ev)
      (event .= ev)

  let pair = [eventType', Cast handler]

  next <- newf $ do
    executor <- newf $ \resolve -> do
      ifelse event
        (do bare $ call1 resolve $ wrap false event
            event .= Null)
        (resolver .= resolve)
    return_ $ newPromise executor

  bare $ call (element !. "addEventListener") pair

  it <- newf $ return_ $ ex "this"
  ret_ <- newf $ do
    bare $ call (element !. "removeEventListener") pair
    bare $ call1 resolver $ wrap true Undefined
    return_ $ wrap true Undefined
  throw_ <- newf $ \err -> do
    return_ $ wrap true $ reject err
  const $ lit
    [ (ex "Symbol" !. "asyncIterator", it)
    , (lit "next", Cast next)
    , (lit "return", ret_)
    , (lit "throw", Cast throw_)
    ]

eventPromise
  :: forall a b e m. (Event e, JS m, MonadFix m)
  => Expr a -> e -> (Expr b -> Expr Bool) -> m (Expr b)
eventPromise el eventType p = do
  executor <- newf $ \resolve -> mdo
    let args = [lit $ eventString eventType, handler]
    handler <- newf $ \ev -> do
      ifonly (p ev) $ do
        bare $ call1 resolve ev
        bare $ call (el !. "removeEventListener") args
    bare $ call (el !. "addEventListener") args
  return $ newPromise executor
