module Web.HTML.Examples where

import Prelude2 hiding (div)
import Web.HTML
import Render

e = div ! href "" $ a ! href "" $ "aeuaeueou"

test = render e
