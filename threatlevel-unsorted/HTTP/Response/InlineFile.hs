module HTTP.Response.InlineFile where

import Common.Prelude
import Data.FileEmbed
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Encoding as TE

import Network.Mime as Mime
import qualified HTTP.Header as Hdr

mkInlineFile path = let
      ct = [| Hdr.header Hdr.ContentType (TL.fromStrict $ TE.decodeUtf8 $ Mime.defaultMimeLookup $ T.pack path) |]
   in [| InlineFile $ct $(embedFile path) |]
