module X.Wai where

import Network.Wai
import Network.HTTP.Types

import X.Prelude

queryText = queryString ^ queryToQueryText
