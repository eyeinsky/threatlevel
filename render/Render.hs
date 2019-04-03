module Render where

import Prelude2
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import Control.Monad ()
import Control.Monad.Reader (Reader, runReader, withReader, ask)

import qualified Text.Exts as X

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
par = f X.par
ang = f X.ang
curly = f X.curly
uncomma = f' X.uncomma
tshow = TL.pack . show
sur a b c = TL.fromStrict (X.sur a b $ TL.toStrict c)
unsemi = f' X.unsemi
q'' = TL.fromStrict . X.q''
f g = TL.fromStrict . g . TL.toStrict
f' g = TL.fromStrict . g . map TL.toStrict
