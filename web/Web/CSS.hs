module Web.CSS where

import Pr
import HTML (TagName(TagName))
import Web.Monad
import CSS

reset = do
  cssRule (TagName "body") zero
  cssRule (TagName "div") zero
  cssRule (TagName "html") (boxSizing "border-box")
  cssRule (TagName "ul") (zero >> listStyle "none")
  cssRule anyTag $ boxSizing "inherit"
  where
    zero = do
      padding $ px 0
      margin $ px 0
