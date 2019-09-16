module HTTP.Response.InlineFile where

import X.Prelude
import HTTP.Response
import Data.FileEmbed
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE

import Network.Mime as Mime
import Network.HTTP.Types (hContentType)
import qualified HTTP.Header as Hdr

mkInlineFile path = let
      ct = [| Hdr.hdr Hdr.ContentType (TL.fromStrict $ TE.decodeUtf8 $ Mime.defaultMimeLookup $ T.pack path) |]
   in [| InlineFile $ct $(embedFile path) |]
