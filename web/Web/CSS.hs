module Web.CSS where

import X.Prelude
import HTML (TagName(TagName))
import Web.Monad
import CSS

reset = do
  cssRule (TagName "html") (boxSizing "border-box")
  cssRule (TagName "ul") (listStyle "none")
  cssRule anyTag $ do
    boxSizing "inherit"
    zero
  where
    zero = do
      padding $ px 0
      margin $ px 0
