module HTML.Blaze.Bootstrap where

import Prelude2
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

import HTML.Blaze

crossorigin = E.customAttribute "crossorigin"
integrity = E.customAttribute "integrity"

-- Latest compiled and minified CSS
css = cssUrl "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  E.! integrity "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"
  E.! crossorigin "anonymous"

-- Optional theme
themeCss = cssUrl "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
  E.! integrity "sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r"
  E.! crossorigin "anonymous"

-- Latest compiled and minified JavaScript
js = do
  jsUrl "https://code.jquery.com/jquery-2.2.4.min.js"
    E.! integrity "sha256-BbhdlvQf/xTY9gja0Dq3HiwQF8LaCRTXxZKRutelT44="
    E.! crossorigin "anonymous"
  jsUrl "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
    E.! integrity "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"
    E.! crossorigin "anonymous"


container = E.div E.! cls_ ["container"]
row = E.div E.! cls_ ["row"]
