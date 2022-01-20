module Web.CSS where

import X.Prelude
import Web.Monad
import CSS

reset :: MonadWeb m => m ()
reset = do
  cssRule (tagSelector "html") (boxSizing "border-box")
  cssRule (tagSelector "ul") (listStyle "none")
  cssRule Any $ do
    boxSizing "inherit"
    zero
  where
    zero = do
      padding $ px 0
      margin $ px 0
