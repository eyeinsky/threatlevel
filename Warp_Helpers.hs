
module Warp_Helpers where

import Prelude2 as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.Wai -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.Warp -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)

import HTTP_Common
import HTTP_URL as U

getRequestBody req = BL.fromChunks <$> loop
   where
      loop = requestBody req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

myRun :: U.Port -> (Request -> IO Response) -> IO ()
myRun (U.Port port) f
   = runSettings (setPort (fromIntegral port) $ defaultSettings) $ \ r r' -> r' =<< f r

newtype BaseURL = BaseURL (U.Proto, U.Host, U.Port)
instance ToPayload BaseURL where
   toPayload (BaseURL (proto, host, port)) =
         toPayload proto
      <> protoSep
      <> toPayload host
      <> portSep
      <> toPayload port
