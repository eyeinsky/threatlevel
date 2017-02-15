
module Warp_Helpers
  ( module Warp_Helpers
  , tlsSettings
  ) where

import Prelude2 as P
import Data.Function (on)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B

import Control.Concurrent

import Network.Wai -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.Warp hiding (getPort) -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings, TLSSettings)
import Network.HTTP.Types

import HTTP.Common
import URL

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = requestBody req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

myRun :: URL.Port -> Handler -> IO ()
myRun (URL.Port port) f
   = runSettings (setPort (fromIntegral port) $ defaultSettings) $ \ r r' -> r' =<< f r

-- * Types

type Handler = Request -> IO Response
type Site = (Authority, IO Handler)

type AppName = String

type AppDef = (AppName, Authority -> IO Handler)
type Rule = (AppName, Authority)

data Server = Server (Maybe (AppDef, Rule, TLSSettings)) [AppDef] [Rule]

-- * Run domains

runDomains :: URL.Port -> [Site] -> IO ()
runDomains port sites = do
  pairs' <- initSites sites
  runSettings
    (mkPort port)
    (\req respond -> let
        domain = fromJust $ getDomain req :: B.ByteString
        resp = snd <$> find ((== domain) . fst) pairs' :: Maybe Handler
      in respond =<< fromMaybe noDomain (($ req) <$> resp)
    )

runDomainTLS :: FilePath -> FilePath -> URL.Port -> Site -> IO ()
runDomainTLS cert key port sites = do
  (_, handler) <- initSite sites
  runTLS tls (mkPort port) (\req resp -> resp =<< handler req)
  where
    tls = tlsSettings cert key

mkPort port = setPort (fromIntegral $ unPort port) $ defaultSettings

-- ** Helpers

initSites sites = forM sites initSite

initSite (a, b) = (toHost a,) <$> b

toHost authority = authority { URL.authentication = Nothing }
   & toPayload
   & TLE.encodeUtf8
   & BL.toStrict

noDomain = return $ responseLBS status404 [] "No host"
getDomain = lookup "Host" . requestHeaders

getPort site = port (site ^. _1)

runServer :: Server -> IO ()
runServer (Server https defs rules) = let
      join :: [(Authority, AppName, IO Handler)]
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

