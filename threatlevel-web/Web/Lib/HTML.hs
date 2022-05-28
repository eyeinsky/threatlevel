module Web.Lib.HTML where

import Common.Prelude
import Data.Time
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import HTML hiding (html)

-- | Convenience class to turn things into @Html@
class ToHtml a where toHtml :: a -> Html

html :: (Profunctor p, Contravariant f, ToHtml a) => Optic' p f a Html
html = to toHtml

instance ToHtml Int where toHtml = show ^ TL.pack ^ text
instance ToHtml String where toHtml = TL.pack ^ text
instance ToHtml Char where toHtml = TL.singleton ^ text
instance ToHtml TS.Text where toHtml = TL.fromStrict ^ text
instance ToHtml TL.Text where toHtml = text
instance ToHtml Html where toHtml a = a


format
  :: (Profunctor p, Contravariant f, FormatTime t)
  => String -> Optic' p f t TL.Text
format str = to (formatTime defaultTimeLocale str ^ TL.pack)

htmlDate :: (Profunctor p, Contravariant f, FormatTime t) => p Html (f Html) -> p t (f t)
htmlDate = format "%F".html

htmlTime :: (Profunctor p, Contravariant f, FormatTime t) => p Html (f Html) -> p t (f t)
htmlTime = format "%F %T".html
