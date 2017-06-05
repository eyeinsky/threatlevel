module Web.Static where

import Pr hiding (head, text)

import Web hiding (content, favicon)
import HTML

import Web.Monad (run)
import Web.Browser

runStatic :: WebT Identity Html -> Document
runStatic wm = Document
  (do cssTag $ renderRaw $ wr^.cssCode <> reset
      jsTag $ renderRaw $ wr^.jsCode
      favicon "data:;base64,iVBORw0KGgo="
      meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8" $ pure ())
  (do body)
  where
    browser = Unknown
    (body :: HTMLM (), _, wr) = runIdentity $ run browser wm
    reset = setBoxSizing browser <> resetCSS browser
