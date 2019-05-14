module X.Wai where

import Control.Monad
import qualified Data.Text as TS
import Network.Wai
import Network.HTTP.Types
import Web.Cookie (parseCookiesText)

import X.Prelude

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

readCookie :: TS.Text -> Request -> Maybe TS.Text
readCookie key = requestHeaders ^ lookup hCookie >=> parseCookiesText ^ lookup key
