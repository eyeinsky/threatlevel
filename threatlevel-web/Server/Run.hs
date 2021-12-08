module Server.Run where

import Common.Prelude
import qualified Data.Text.Lazy as TL
import Network.Wai.Handler.Warp as Warp hiding (getPort)
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Network.Wai as Wai

import qualified Web.Monad as WM
import qualified HTTP.Response as HR
import qualified Server.Response as WR
import qualified Server.API as API
import URL

type SiteTypePrim r a
   = WM.Conf
  -> WM.State
  -> URL
  -> Warp.Settings
  -> r
  -> API.T r
  -> a

type SiteType r a
  = (API.Confy r)
  => WM.Conf
  -> WM.State
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
            response@ (WR.Response {}) -> respond $ HR.toRaw response
            WR.WebSocket ws ->
              WS.websocketsOr WS.defaultConnectionOptions ws
                 (error "This should never happen")
                 req respond
            file@ (WR.File {}) -> respond $ HR.toRaw file
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
siteMain maybeTls = siteMain' maybeTls def def
