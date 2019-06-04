module X.Wai
  ( module X.Wai
  , module Export
  ) where

import Network.Wai as Export hiding (Response)
import Network.HTTP.Types as Export
import Network.Wai (Request)

import Control.Monad
import qualified Data.Text as TS
import qualified Data.ByteString as BS
import Web.Cookie (parseCookiesText)

import X.Prelude

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

readCookie :: TS.Text -> Request -> Maybe TS.Text
readCookie key = requestHeaders ^ lookup hCookie >=> parseCookiesText ^ lookup key

listCookies :: Request -> Maybe BS.ByteString
listCookies = requestHeaders ^ lookup hCookie
