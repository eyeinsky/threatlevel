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
