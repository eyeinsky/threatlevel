module TH where

import Prelude2
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH

kebab2camel t = TL.unpack $ TL.concat $ x : map capitalise xs
  where
    x : xs = TL.splitOn "-" t
    capitalise t = let (a, b) = TL.splitAt 1 t
       in TL.toUpper a <> b

reserved_ t = if t `elem` ["class", "type", "in"]
  then t <> "_"
  else t

mk t name = [d| $(varP $ mkName name) = \c -> tell [tag $(stringE name) & (contents .~ execWriter (c :: $t )) ] :: $t |]

mkAttr custom type_ name = [d| $(varP $ mkName name) = \a -> $(conE custom) $(stringE name) a :: $type_ |]
