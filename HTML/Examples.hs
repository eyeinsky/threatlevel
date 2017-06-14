module HTML.Examples where

import Prelude2 hiding (div, on)
import HTML
import Render
import DOM
import qualified JS

e = div
  ! href "a"
    $ a
    ! on Click (JS.ex "f")
    ! href "b"
    ! AttrId (Id "iiiidddeee")
    $ "aeuaeueou"

test = render e
