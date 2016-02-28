module HTTP_Response_InlineFile where

import Prelude2
import HTTP_Response
import Data.FileEmbed
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE

import Network.Mime as Mime
import Network.HTTP.Types (hContentType)
import qualified HTTP_Header as Hdr

mkInlineFile path = let
      ct = [| Hdr.hdr Hdr.ContentType (TL.fromStrict $ TE.decodeUtf8 $ Mime.defaultMimeLookup $ T.pack path) |]
   in [| InlineFile $ct $(embedFile path) |]
