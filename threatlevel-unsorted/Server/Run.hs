module Server.Run where

import Common.Prelude
import qualified Data.Text.Lazy as TL
import Network.Wai.Handler.Warp as Warp hiding (getPort)
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Network.Wai as Wai

import CSS qualified
import JS qualified
import Web.DSL qualified as WM
import HTTP.Response qualified as HR
import Server.Response qualified as WR
import Server.API qualified as API
import URL

type SiteTypePrim r a
   = WM.Reader_
  -> WM.State_
  -> URL
  -> Warp.Settings
  -> r
  -> API.T r
  -> a

type SiteType r a
  = (API.Confy r)
  => WM.Reader_
  -> WM.State_
  -> URL
  -> Warp.Settings
  -> r
  -> API.T r
  -> a

siteMain' :: Maybe Warp.TLSSettings -> SiteType r (IO ())
siteMain' maybeTls mc ms siteRoot settings env site = do
  handler <- API.toHandler mc ms siteRoot env site
  let handler' req respond = do
        r :: Maybe WR.Response <- handler req
        case r of
          Just r' -> case r' of
            response@(WR.Response {}) -> respond $ HR.toRaw response
            WR.WebSocket ws ->
              WS.websocketsOr WS.defaultConnectionOptions ws
                 (error "This should never happen")
                 req respond
            file@(WR.File {}) -> respond $ HR.toRaw file
          _ -> do
            print $ "Path not found: " <> show (Wai.pathInfo req)
              <> ", URL: " <> TL.unpack (render' siteRoot)
            respond $ HR.toRaw $ WR.htmlDoc "" "Page not found"
  case maybeTls of
    Just tls -> Warp.runTLS tls settings handler'
    _ -> Warp.runSettings settings handler'


type SiteType' r a
  = (API.Confy r)
  => URL
  -> Warp.Settings
  -> r
  -> API.T r
  -> a

siteMain :: Maybe Warp.TLSSettings -> SiteType' r (IO ())
siteMain maybeTls = siteMain' maybeTls (WM.hostSelector, JS.Indent 2) (CSS.identifiers, def)
