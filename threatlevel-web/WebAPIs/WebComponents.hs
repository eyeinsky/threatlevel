module WebAPIs.WebComponents where

import qualified Data.Text as TS
import X.Prelude hiding ((>))

import HTML hiding (method)
import JS
import JS.Syntax (Name)
import qualified DOM

-- * Custom element registry

customElements :: Expr a
customElements = ex "customElements"

define :: [Expr a] -> Expr ()
define args = call (customElements !. "define") args

whenDefined :: Function f => Expr TS.Text -> f -> M r ()
whenDefined name f = do
  f' <- async f
  bare $ (customElements !// "whenDefined" $ name) !// "then" $ f'

upgrade :: Expr el -> Expr ()
upgrade root = call1 (customElements !. "upgrade") root

-- * Lifecycle callbacks

connectedCallback, disconnectedCallback, adoptedCallback, attributeChangedCallback, observedAttributes
  :: Function fexp => fexp -> ClassBodyM

connectedCallback = method "connectedCallback"
disconnectedCallback = method "disconnectedCallback"
adoptedCallback = method "adoptedCallback"
attributeChangedCallback = method "attributeChangedCallback"
observedAttributes = staticGet "observedAttributes"

-- * API

-- | Create autonomous custom element
customElement :: String -> ClassBodyM -> M r ()
customElement elementName code = do
  className <- JS.next
  cls <- classExtends className (Just "HTMLElement") code
  bare $ define [lit elementName, cls]

-- | Use custom element
custom :: DOM.Value -> Html -> Html
custom name content = tell [el]
  where el = tag name & contents .~ execWriter content

-- https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define
-- get, upgrade, whenDefined
