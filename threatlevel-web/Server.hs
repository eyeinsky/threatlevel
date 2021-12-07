{-# OPTIONS_GHC -Wno-orphans #-}
module Server where

import Common.Prelude
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import System.IO.Unsafe
import System.Environment

import Network.Wai
import Network.Wai.Handler.Warp as Warp hiding (getPort)
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import Network.Wai as Wai


import qualified Web.Monad as WM
import qualified HTTP.Response as HR
import qualified Web.Response as WR
import qualified Web.Endpoint as WE

import URL

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

-- * Helpers

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = getRequestBodyChunk req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

getDomain :: Request -> Maybe B.ByteString
getDomain = lookup "Host" . requestHeaders

reqCookie :: Request -> Maybe B.ByteString
reqCookie = lookup "Cookie" . requestHeaders

-- ** TLS settings with certificate

tlsSettingsEnvIO :: String -> String -> IO (Maybe Warp.TLSSettings)
tlsSettingsEnvIO cert key = do
  certPath <- lookupEnv cert
  keyPath <- lookupEnv key
  return $ Warp.tlsSettings <$> certPath <*> keyPath

tlsSettingsEnv :: String -> String -> Maybe Warp.TLSSettings
tlsSettingsEnv cert key = unsafePerformIO $ tlsSettingsEnvIO cert key
