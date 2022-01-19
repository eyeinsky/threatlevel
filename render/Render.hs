module Render where

import Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import Control.Monad ()
import Control.Monad.Reader (Reader, runReader)

-- * Monad

type Text = TL.Text

class Render a where
  type Conf a
  type Conf a = ()
  renderM :: a -> Reader (Conf a) Text

render :: forall a. (Render a) => Conf a -> a -> Text
render conf a = runReader (renderM a) conf

render' :: forall a. (Render a, Conf a ~ ()) => a -> Text
render' a = runReader (renderM a) ()

printRender :: Render a => Conf a -> a -> IO ()
printRender conf a = TLIO.putStr $ render conf a

renderBS :: Render a => Conf a -> a -> BL.ByteString
renderBS conf = TLE.encodeUtf8 . render conf

-- ** Helpers

mseq :: (Monoid b, Monad f) => [f b] -> f b
mseq li = mconcat <$> sequence li

(<+>) :: (Monoid b, Applicative f) => f b -> f b -> f b
a <+> b = (<>) <$> a <*> b

-- * Rebinds for own helpers

sur :: TL.Text -> TL.Text -> TL.Text -> TL.Text
sur begin end cont = begin <> cont <> end

par :: TL.Text -> TL.Text
par = sur "(" ")"

curly :: TL.Text -> TL.Text
curly = sur "{" "}"

ang :: TL.Text -> TL.Text
ang = sur "[" "]"

uncomma :: [TL.Text] -> TL.Text
uncomma = TL.intercalate ","

unsemi :: [TL.Text] -> TL.Text
unsemi =  TL.intercalate ";"

tshow :: Show a => a -> TL.Text
tshow = TL.pack . show

f :: (TS.Text -> TS.Text) -> TL.Text -> TL.Text
f g = TL.fromStrict . g . TL.toStrict

f' :: ([TS.Text] -> TS.Text) -> [TL.Text] -> TL.Text
f' g = TL.fromStrict . g . map TL.toStrict
