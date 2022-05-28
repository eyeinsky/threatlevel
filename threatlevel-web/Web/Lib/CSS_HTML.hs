{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Lib.CSS_HTML where

import Common.Prelude
import qualified Data.Text.Lazy as TL
import Control.Monad.Writer
import CSS
import HTML hiding (style)

getTag :: XMLM ns c -> SimpleSelector
getTag a = case execWriter a of
  e : _ -> let
      tn = e^?_Element._1 :: Maybe TagName
    in maybe (err "Not elem") ssFrom tn
  _ -> err "No xml content"
  where
    prefix = "SimpleSelectorFrom (XMLM ns c -> XMLM ns c): "
    err msg = error $ prefix <> msg

instance SimpleSelectorFrom (XMLM ns c -> XMLM ns c) where
  ssFrom a = getTag $ a $ pure ()

instance SimpleSelectorFrom (XMLM ns c) where
  ssFrom a = getTag a

decls :: MonoProp () -> Attribute
decls = style
{-# DEPRECATED decls "Use `style` instead." #-}

style :: MonoProp () -> Attribute
style decls = Custom "style" (Static $ TL.toStrict $ render CSS.Minify decls)
