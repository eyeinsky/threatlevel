{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Lib.HTML_URL where

import Common.Prelude
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import DOM.Core qualified as DOM
import URL
import HTML qualified
import HTML hiding (src, for, href)
import Web.Lib.HTML

-- * HTML + URL

favicon :: URL.URL -> Html
favicon url = link ! rel "icon" ! href url

faviconSvg :: URL -> Html
faviconSvg url = link ! rel "icon" ! href url ! Custom "sizes" "any" ! type_ "image/svg+xml"

href :: URL.URL -> Attribute
href url = HTML.href (Static $ TL.toStrict $ render' url)

stylesheet :: URL.URL -> Html
stylesheet url = link ! rel "stylesheet" ! type_ "text/css" ! href url

stylesheet_ :: TS.Text -> Html
stylesheet_ url = link ! rel "stylesheet" ! type_ "text/css" ! HTML.href (Static url)

includeJs :: URL.URL -> Html
includeJs url = script ! src url $ "" ! Custom "defer" "true"

includeJs_ :: TS.Text -> Html
includeJs_ url = script ! HTML.src (Static url) $ "" ! Custom "defer" "true"

-- | Include JavaScript module from @url@ [api]
includeModule :: URL -> Html
includeModule url = script "" ! src url ! type_ "module"

-- | Include JavaScript module from untyped @url@ [api]
includeModule_ :: TS.Text -> Html
includeModule_ url = script "" ! HTML.src (Static url) ! type_ "module"

-- | Helper to turn attribute into URL
urlAttr :: URL.URL -> DOM.Value
urlAttr url = Static $ TL.toStrict $ render' url

src :: URL.URL -> Attribute
src url = HTML.src (urlAttr url)

action :: URL.URL -> Attribute
action url = HTML.action (urlAttr url)

for :: DOM.Id -> Attribute
for id = HTML.for (coerce id)

instance ToHtml URL.URL where toHtml = renderURL ^ text
