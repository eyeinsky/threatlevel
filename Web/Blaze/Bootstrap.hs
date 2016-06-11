module Web.Blaze.Bootstrap
  (css, themeCss, js) where

import Prelude2
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

crossorigin = E.customAttribute "crossorigin"
integrity = E.customAttribute "integrity"
(!) = (E.!)

-- Latest compiled and minified CSS
css = do
  E.link
    ! A.rel "stylesheet"
    ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
    ! integrity "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"
    ! crossorigin "anonymous"

-- Optional theme
themeCss = do
  E.link
    ! A.rel "stylesheet"
    ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
    ! integrity "sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r"
    ! crossorigin "anonymous"

-- Latest compiled and minified JavaScript
js = do
  E.script
    ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
    ! integrity "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"
    ! crossorigin "anonymous"
    $ ""
