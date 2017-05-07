module Web.Static where

import Prelude2 hiding (head, text)
import qualified Data.Text.Lazy as TL

import JS
import Web hiding (content)
import HTML

import Web.Monad (run)
import Web.Browser

runStatic :: WebT Identity (HTMLM ()) -> HTMLM ()
runStatic wm = html $ do
  head $ do
    cssTag $ renderRaw $ wr^.cssCode <> reset
    jsTag $ renderRaw $ wr^.jsCode
    favicon "data:;base64,iVBORw0KGgo="
    meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8" $ pure ()
  body
  where
    browser = Unknown
    (body :: HTMLM (), _, wr) = runIdentity $ run browser wm
    reset = setBoxSizing browser <> resetCSS browser


cssTag :: HTMLM () -> HTMLM ()
cssTag = style ! type_ "text/css"

jsTag :: HTMLM () -> HTMLM ()
jsTag = script ! type_ "text/javascript"

favicon :: TL.Text -> HTMLM ()
favicon adr = link
  ! rel "shortcut icon"
  ! type_ "image/x-icon"
  ! href adr
  $ pure ()
