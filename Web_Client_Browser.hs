module Web_Client_Browser where

import Prelude2

import qualified Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B


{- | Detect web browser from User-Agent HTTP header.

     This is the main entry point of directing correct
     code to browsers.
-}

parseBrowser :: B.ByteString -> Browser
parseBrowser v
   | f "Internet Explorer"      = IE
   | f "Firefox"                = Firefox
   | f "Chrome" || f "Chromium" = Chrome
   | otherwise                  = Unknown
   where
      enc = TE.decodeUtf8 v
      f x = let len = T.length x
         in T.tails enc
          & map (T.zip x)
          & takeWhile (eq len . length)
          & map (and . map (uncurry eq))
          & or

data Browser
   = Chrome
   | Firefox
   | IE
   | Opera
   | Unknown
   deriving (Eq, Show)




