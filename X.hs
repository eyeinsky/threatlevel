module X
  ( module X
  , module Export
  ) where

import HTML as Export hiding (
  -- redefined
  href,
  -- used in CSS
  em, font, content, Value,
  )
import CSS as Export hiding (
  -- generic
  filter, all, transform
  )
import Web.Monad as Export -- hiding (href, em, font, content, Value, all, transform, filter)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as LL

import Web.Cookie (parseCookiesText)
import Network.Wai as Wai
import qualified Network.HTTP.Types as Wai

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR

import Prelude2 as P
import Data.Default
import Render
import JS

import qualified URL
import qualified HTML
import qualified DOM
import qualified Web.Response as WR


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
  WR.Response s h (WR.Raw bl) -> WR.Response s (f h) $ WR.Raw bl
  _ -> req

deleteCookie :: TS.Text -> WR.Response -> WR.Response
deleteCookie key = withHeaders (Hdr.delC (TL.fromStrict key) :)

setCookie :: TS.Text -> TS.Text -> WR.Response -> WR.Response
setCookie k v = withHeaders (setCookie (TL.fromStrict v))
  where
    setCookie :: TL.Text -> [Hdr.Header] -> [Hdr.Header]
    setCookie val = (Hdr.cookie' (TL.fromStrict k) val Nothing (Just []) Nothing :)

hasCookie :: TS.Text -> TS.Text -> Wai.Request -> P.Bool
hasCookie k v req = req
  & Wai.requestHeaders
  ^ lookup Wai.hCookie
  ^ fmap parseCookiesText
  ^ maybe False (lookup k ^ maybe False (v ==))
