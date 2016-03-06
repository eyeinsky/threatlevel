
module Warp_Helpers where

import Prelude2 as P
import Data.Function (on)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B

import Control.Concurrent

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

type Site = (Authority, AppIO)
runDomains :: URL.Port -> [Site] -> IO ()
runDomains port sites = do
   pairs' <- forM sites $ \ (a, b) -> (toHost a,) <$> b
   runSettings (setPort (fromIntegral $ unPort port) $ defaultSettings) $
      \ req respond -> let
            domain = fromJust $ getDomain req :: B.ByteString
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

-- type HostMatch = Maybe Authority
type AppName = String
type AppIO = IO (Request -> IO Response)
type App = Authority -> AppIO
type AppDef = (AppName, App)
type Rule = (AppName, Authority)

runServer :: [AppDef] -> [Rule] -> IO ()
runServer defs rules = let
      join :: [(Authority, AppName, AppIO)]
      join = do
         (name, app) <- defs
         (name', autho) <- rules
         if name == name'
            then return (autho, name, app autho)
            else []
      portSites :: [(URL.Port, [Site])]
      portSites = join
         & map (\ j -> (j^._1, j^._3) :: Site)
         & sortBy (compare `on` getPort)
         & groupBy (eq `on` getPort)
         & map pullPort
   in do
      pr join
      case portSites of
         x : xs -> let f = uncurry runDomains
            in mapM_ (forkIO . f) xs >> f x
         _ -> print "Nothing to run"
   where
      pullPort xs@ (x : _) = (getPort x, xs)
      getPort site = port (site ^. _1)
      pr = mapM_ (\j -> print (j^._1 ,j^._2))

newtype BaseURL = BaseURL (URL.Proto, URL.Host, URL.Port)
instance ToPayload BaseURL where
   toPayload (BaseURL (proto, host, port)) =
         toPayload proto
      <> protoSep
      <> toPayload host
      <> portSep
      <> toPayload port
