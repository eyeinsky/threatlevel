module HTTP_Util where

import Prelude2
import qualified Data.Text.Lazy as TL
import HTTP_Request
import HTTP_Response
import HTTP_URL

bareDomain dom = URL
   (Proto "http")
   (Authority Nothing (Domain dom) (Port 80))
   (Path []) (Params []) (Fragment "")
bareIp a b c d = URL
   (Proto "http") (Authority Nothing (IP4 a b c d) (Port 80))
   (Path []) (Params []) (Fragment "")
