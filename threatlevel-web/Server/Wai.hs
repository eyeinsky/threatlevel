module Server.Wai where

import Common.Prelude
import Data.Text qualified as TS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.Environment
import System.IO.Unsafe
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai
import Network.HTTP.Types
import Web.Cookie

-- * Request

-- ** Headers

getDomain :: Request -> Maybe BS.ByteString
getDomain = lookup "Host" . requestHeaders

reqCookie :: Request -> Maybe BS.ByteString
reqCookie = lookup "Cookie" . requestHeaders

hasCookie :: TS.Text -> TS.Text -> Request -> Bool
hasCookie k v = getCookie k ^ maybe False (v ==)

getCookie :: TS.Text -> Request -> Maybe TS.Text
getCookie k = requestCookies >=> lookup k

requestCookies :: Request -> Maybe CookiesText
requestCookies = requestHeaders
  ^ lookup hCookie
  ^ fmap parseCookiesText

-- ** Params

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

queryParam' :: TS.Text -> Request -> Maybe (Maybe TS.Text)
queryParam' name req = lookup name $ queryText req

queryParam :: TS.Text -> Request -> Maybe TS.Text
queryParam name req = join $ queryParam' name req

hasParam :: TS.Text -> Request -> Bool
hasParam name req = isJust $ lookup name $ queryText req

-- ** Body

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = getRequestBodyChunk req >>= re
      re chunk = BS.null chunk ? return [""] $ (chunk:) <$> loop

-- | Get @request@ POST body as QueryText
postKvs :: Request -> IO QueryText
postKvs request = do
  bs <- getRequestBody request
  return $ parseQueryText $ BL.toStrict bs

-- ** TLS settings with certificate

tlsSettingsEnvIO :: String -> String -> IO (Maybe Warp.TLSSettings)
tlsSettingsEnvIO cert key = do
  certPath <- lookupEnv cert
  keyPath <- lookupEnv key
  return $ Warp.tlsSettings <$> certPath <*> keyPath

tlsSettingsEnv :: String -> String -> Maybe Warp.TLSSettings
tlsSettingsEnv cert key = unsafePerformIO $ tlsSettingsEnvIO cert key
