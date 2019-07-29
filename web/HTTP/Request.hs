module HTTP.Request where

import Prelude2

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.Wai
import Network.HTTP.Types

-- * Wai Request

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

-- textFormSubmission :: Request -> IO [(Maybe TL.Text, Maybe (Maybe TL.Text))]
formSubmission :: Request -> IO [(BL.ByteString, Maybe BL.ByteString)]
formSubmission req = do
  lb <- strictRequestBody req
  let pairs = BL8.split '&' lb :: [BL.ByteString] -- queryString
      f (k, v) = case v of
        "" -> (k, Nothing)
        _ -> (k, Just $ BL8.tail v)
      g = strict %~ urlDecode True
      decode (k, v) = (g k, g <$> v)
  return $ map (decode . f . BL8.break (== '=')) pairs

textFormSubmission :: Request -> IO [(TL.Text, Maybe TL.Text)]
textFormSubmission req = do
  formSubmission req <&> map (f *** fmap f)
  where
    f = view TL.utf8
