module JS.WebApis.DOM
  ( module JS.WebApis.DOM
  , module JS.WebApis.Event
  ) where

import Common.Prelude
import Data.Text qualified as TS
import CSS qualified
import JS
import HTML

import JS.WebApis.Event


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

-- * Modify

appendChild :: Expr Tag -> Expr Tag -> Expr ()
appendChild a t = call1 (t !. "appendChild") a

createDocumentFragment :: Expr DocumentFragment
createDocumentFragment = call0 (document !. "createDocumentFragment")

insertBefore a b = call (parentNode b !. "insertBefore") [a, b]

replaceChild old new = call (parentNode old !. "replaceChild") [new, old]

removeChild :: Expr Tag -> Expr Tag -> Expr Tag
removeChild parent child = call1 (parent !. "removeChild") child

parentNode :: Expr Tag -> Expr Tag
parentNode e = e !. "parentNode"

createElement :: TagName -> Expr Tag
createElement tn = document !// "createElement" $ lit $ coerce @_ @Value tn

createTextNode :: Expr String -> Expr Tag
createTextNode txt = document !// "createTextNode" $ txt

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
  lit <- func Func $ bare $ addEventListener (Cast element) type_ handler'
  bare $ addEventListener (Cast window) Load lit
  return $ Cast handler'
