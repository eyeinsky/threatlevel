module HTTP.Util where

import X.Prelude
import qualified Data.Text.Lazy as TL
import HTTP.Request
import HTTP.Response
import URL

bareDomain dom = URL
   (Proto "http")
   (Authority Nothing (Domain dom) (Port 80))
   (Path []) (Params []) (Fragment "")
bareIp a b c d = URL
   (Proto "http") (Authority Nothing (IP4 a b c d) (Port 80))
   (Path []) (Params []) (Fragment "")
