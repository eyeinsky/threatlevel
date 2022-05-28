{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Lib.JS_HTML where

-- import Common.Prelude
-- import Data.Text qualified as TS
import JS
import HTML
import Web.Lib.HTML

instance ToHtml (Expr Tag) where
  toHtml a = HTML.dyn a
instance ToHtml (Expr DocumentFragment) where
  toHtml a = HTML.dyn a
-- instance ToHtml (Expr String) where
--   toHtml a = HTML.dyn $ createTextNode a
-- instance ToHtml (Expr TS.Text) where
--   toHtml a = HTML.dyn $ createTextNode $ Cast a
-- instance ToHtml (Expr Int) where
--   toHtml e = dyn $ createTextNode $ toString e
