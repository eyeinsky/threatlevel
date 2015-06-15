
module Warp_Helpers where

import Prelude2 as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Network.Wai -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.Warp -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)

getRequestBody req = BL.fromChunks <$> loop
   where
      loop = requestBody req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

myRun :: Int -> (Request -> IO Response) -> IO ()
myRun port f = runSettings (setPort port $ defaultSettings) $ \ r r' -> r' =<< f r
