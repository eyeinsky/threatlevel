module Web.Lib.URL_CSS where

import Common.Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import URL
import CSS

fontSrc :: URL -> Maybe TS.Text -> Value
fontSrc url mbFmt
  = Url (TL.toStrict $ renderURL url) <> maybe "" f mbFmt
  where
    f fmt = Word $ "format(\"" <> fmt <> "\")"
