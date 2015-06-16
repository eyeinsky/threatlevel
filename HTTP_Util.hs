module HTTP_Util where

import Prelude2
import qualified Data.Text.Lazy as TL
import HTTP_Request
import HTTP_Response
import HTTP_URL


get :: TL.Text -> Request a  
get urlT = GET (url, hdrsFrom url)
   where url = bareDomain urlT

getIp a b c d = GET (url, hdrsFrom url)
   where url = bareIp a b c d

post :: URL -> Params -> Request a
post = u


-- * Convenience

bareDomain dom = URL
   (Proto "http") (Domain dom) (Port 80)
   (Path []) (Params []) (Fragment "")
bareIp a b c d = URL
   (Proto "http") (IP4 a b c d) (Port 80)
   (Path []) (Params []) (Fragment "")

hdrsFrom url = u {- z <> (maybe [] (\x -> [hdr Hdr.Host x]) hst) :: [Hdr.Header]
   where
      z = [ hdr Hdr.Accept H.accHtml ]
      hst = case host url of
         Domain d    -> Just d
         IP4 _ _ _ _ -> Nothing
-}

