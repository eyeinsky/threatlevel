module XML.TH where

import qualified Data.Text.Lazy as TL
import Language.Haskell.TH

import Common.TH

import X.Prelude

mk t name = [d|
  $(varP $ mkName name) = (\c -> let
     el = tag $(stringE name) & contents .~ execWriter c
     in tell [el] :: $t) :: $t -> $t
  |]

mkAttr custom type_ name = [d|
  $(varP . mkName . TL.unpack . kebab2camel . reserved_ $ name) = \a ->
    $(conE custom) $(stringE $ TL.unpack name) a :: $type_
  |]
