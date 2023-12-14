import Prelude

import Data.Text
import Criterion.Main
import Criterion.Types

import Identifiers

main = defaultMain
  [ bgroup "startFrom' \"a\" :: [String]"
    [ bench "!! 500" $ nf (startFrom' "" !!) 500
    , bench "!! 5000" $ nf (startFrom' "" !!) 5000
    , bench "!! 50000" $ nf (startFrom' "" !!) 50000
    ]
  , bgroup "startFrom \"a\" :: [T.Text]"
    [ bench "!! 500" $ nf (startFrom "" !!) 500
    , bench "!! 5000" $ nf (startFrom "" !!) 5000
    , bench "!! 50000" $ nf (startFrom "" !!) 50000
    ]
  ]
