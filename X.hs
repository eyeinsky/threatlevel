module X where

import Prelude2
import Data.Default
import Render
import JS
import HTML
import qualified DOM


-- * DOM.Event

-- | Create inline on-event attribute
on :: DOM.Event e => e -> Expr a -> Attribute
on event handler = Custom (DOM.toOn event) (render def $ call1 handler $ ex "event")
  where
    -- JS.Syntax.EName (JS.Syntax.Name handler') = handler
    -- ^ todo: find a generic way to get the name, even for literal
    -- expressinos.

-- * HTML

href :: URL.URL -> Attribute
href url = HTML.href (WR.renderURL url)

-- * HTTP.Response

-- todo: make this a lens for headers on AnyResponse
withHeaders f req = case req of
  WR.Raw s h b -> WR.Raw s (f  h) b
  _ -> req

deleteCookie :: TS.Text -> WR.AnyResponse -> WR.AnyResponse
deleteCookie key = withHeaders (Hdr.delC (TL.fromStrict key) :)

setCookie :: TS.Text -> TS.Text -> WR.AnyResponse -> WR.AnyResponse
setCookie k v = withHeaders (setCookie (TL.fromStrict v))
  where
    setCookie :: TL.Text -> [Hdr.Header] -> [Hdr.Header]
    setCookie val = (Hdr.cookie' (TL.fromStrict k) val Nothing (Just []) Nothing :)
