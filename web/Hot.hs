module Hot where

import qualified Data.Text.Lazy as TL
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Rapid

import X.Prelude
import qualified Web.Monad as WM
import qualified HTTP.Response as HR
import qualified Web.Response as WR
import qualified Web.Endpoint as WE
import URL

updated :: Ord k => k -> IO () -> IO ()
updated name main = rapid 1 (\r -> restart r name main)

mkHot :: Ord k => k -> IO () -> (IO (), IO ())
mkHot name what = let
  reload = updated name what
  stop_ = rapid 1 (\r -> stop r name what)
  in (reload, stop_)

type SiteType r a
  = (WE.Confy r)
  => WM.Conf
  -> WM.State
  -> URL
  -> Warp.Settings
  -> r
  -> WE.T r
  -> a

siteMain' :: Maybe Warp.TLSSettings -> SiteType r (IO ())
siteMain' maybeTls mc ms siteRoot settings env site = do
  handler <- WE.toHandler mc ms siteRoot env site
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
  = (WE.Confy r)
  => URL
  -> Warp.Settings
  -> r
  -> WE.T r
  -> a

siteMain :: Maybe Warp.TLSSettings -> SiteType' r (IO ())
siteMain maybeTls = siteMain' maybeTls def def

type HotType k r a = (Ord k) => k -> SiteType r a

mkHotPrim :: Maybe Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
mkHotPrim maybeTls name mc ms siteRoot settings env site = (hot, stop, main)
  where
    main = siteMain' maybeTls mc ms siteRoot settings env site
    (hot, stop) = mkHot name main

hotHttp :: HotType k r (IO (), IO (), IO ())
hotHttp = mkHotPrim Nothing

hotHttps :: Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
hotHttps tls = mkHotPrim (Just tls)
