module Server.Wai where

import Common.Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.Environment
import System.IO.Unsafe
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai

-- * Helpers

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = getRequestBodyChunk req >>= re
      re chunk = BS.null chunk ? return [""] $ (chunk:) <$> loop

getDomain :: Request -> Maybe BS.ByteString
getDomain = lookup "Host" . requestHeaders

reqCookie :: Request -> Maybe BS.ByteString
reqCookie = lookup "Cookie" . requestHeaders

-- ** TLS settings with certificate

tlsSettingsEnvIO :: String -> String -> IO (Maybe Warp.TLSSettings)
tlsSettingsEnvIO cert key = do
  certPath <- lookupEnv cert
  keyPath <- lookupEnv key
  return $ Warp.tlsSettings <$> certPath <*> keyPath

tlsSettingsEnv :: String -> String -> Maybe Warp.TLSSettings
tlsSettingsEnv cert key = unsafePerformIO $ tlsSettingsEnvIO cert key
