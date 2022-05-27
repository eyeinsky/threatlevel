module JS.WebApis.WebComponents where

import qualified Data.Text as TS
import X.Prelude hiding ((>))

import JS
-- import qualified DOM

-- * Custom element registry

customElements :: Expr a
customElements = ex "customElements"

define :: [Expr a] -> Expr ()
define args = call (customElements !. "define") args

whenDefined :: JS m => Function f m => Expr TS.Text -> f -> m ()
whenDefined name f = do
  f' <- async f
  bare $ (customElements !// "whenDefined" $ name) !// "then" $ f'

upgrade :: Expr el -> Expr ()
upgrade root = call1 (customElements !. "upgrade") root

-- * Lifecycle callbacks

connectedCallback, disconnectedCallback, adoptedCallback, attributeChangedCallback, observedAttributes
  :: JS m => Function fexp m => fexp -> ClassBodyM m

connectedCallback = method "connectedCallback"
disconnectedCallback = method "disconnectedCallback"
adoptedCallback = method "adoptedCallback"
attributeChangedCallback = method "attributeChangedCallback"
observedAttributes = staticGet "observedAttributes"

-- * API

-- | Create autonomous custom element
customElement :: JS m => String -> ClassBodyM m -> m (Expr a)
customElement elementName code = do
  className <- JS.freshName
  cls <- classExtends className (Just "HTMLElement") code
  bare $ define [lit elementName, cls]
  pure cls

-- https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define
-- get, upgrade, whenDefined
