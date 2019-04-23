module X.Google.Maps where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Data.List
import qualified Data.HashMap.Lazy as HM

import Prelude2 hiding ((?=))
import URL

-- * JS




-- * Maps Static API

type Submap = [(TS.Text, Maybe TS.Text)]
type Val = Either TS.Text Submap
type StaticMap = [(TS.Text, Val)]

staticApi :: URL
staticApi = URL.URL "https"
  (URL.Authority Nothing "maps.googleapis.com" 443)
  (URL.Path ["maps", "api", "staticmap"])
  (URL.Params [])
  (URL.Fragment "")

renderStaticUrl :: StaticMap -> URL
renderStaticUrl hm = foldr f staticApi hm
    where
      f :: (TS.Text, Val) -> URL -> URL
      f (k, v) url = url & k ?= either id g v
      g :: Submap -> TS.Text
      g m = let
        (props, coords) = partition (snd ^ isJust) m
        h (k, v) a = a <> pure (
          TL.fromStrict k <> maybe "" (TL.fromStrict ^ (":" <>)) v)
        in foldr h [] (coords <> props)
           & intersperse "|"
           & mconcat
           & TL.toStrict

prop a b = (a, Left b)
submap a b = (a, Right b)

coords = "59.3405729, 18.0639158"
addr = "Tegnérgatan 3, 111 40 Stockholm"
cur = coords
test = renderStaticUrl
  [ prop "size" "640x240"
  , prop "scale" "2"
  , prop "zoom" "11"
--  , prop "center" cur
  , submap"markers" [(cur, Nothing), ("size", Just "tiny")]
  , submap"markers" [("59.328902,18.0284233", Nothing), ("size", Just "tiny"), ("color", Just "blue")]
  , submap"style" [("saturation", Just "-100")]
  , prop "key" "GOOGLE_API_KEY"
  ]
