module XML.TH where

import Common.Prelude
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH

import Common.TH

import X.Prelude
import qualified DOM.Core

mk :: TypeQ -> String -> DecsQ
mk type_ name = let name' = mkName name
  in declareFn name' [t| $type_ -> $type_ |] [| tag $(stringE name) |]

mkAttr :: Name -> TypeQ -> TL.Text -> DecsQ
mkAttr custom type_ name = let
  name' = mkName $ TL.unpack $ tlKebab2camel $ reserved_ $ name
  in declareFn name'
       [t| DOM.Core.Value -> $type_ |]
       [| \a -> $(conE custom) $(stringE $ TL.unpack name) a |]
