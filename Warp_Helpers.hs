
module Warp_Helpers where

import Prelude2 as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import Network.Wai -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.Warp -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.HTTP.Types

import HTTP_Common
import URL

getRequestBody req = BL.fromChunks <$> loop
   where
      loop = requestBody req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

myRun :: URL.Port -> (Request -> IO Response) -> IO ()
myRun (URL.Port port) f
   = runSettings (setPort (fromIntegral port) $ defaultSettings) $ \ r r' -> r' =<< f r

runDomains :: URL.Port -> [(Maybe URL.Authority, IO (Request -> IO Response))] -> IO ()
runDomains (URL.Port port) sites
   = do
      -- Listening on: mapM (print . fst) sites
      pairs' <- forM sites $ \ (a, b) -> (toHost <$> a,) <$> b
      runSettings (setPort (fromIntegral port) $ defaultSettings) $
         \ req respond -> let
               domain = getDomain req :: Maybe B.ByteString
               resp = snd <$> find ((== domain) . fst) pairs' :: Maybe (Request -> IO Response)
            in do
            respond =<< fromMaybe noDomain (($ req) <$> resp)
   where
      getDomain = lookup "Host" . requestHeaders
      noDomain = return $ responseLBS status404 [] "No host"
      toHost authority = authority { URL.authentication = Nothing }
         & toPayload
         & TLE.encodeUtf8
         & BL.toStrict

newtype BaseURL = BaseURL (URL.Proto, URL.Host, URL.Port)
instance ToPayload BaseURL where
   toPayload (BaseURL (proto, host, port)) =
         toPayload proto
      <> protoSep
      <> toPayload host
      <> portSep
      <> toPayload port
