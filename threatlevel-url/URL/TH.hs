module URL.TH where

import Prelude
import Data.Word
import Control.Lens
import qualified Data.Text as TS
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import URL.Core
import URL.Parse


url :: QuasiQuoter
url = QuasiQuoter
  { quoteExp = \str -> case parseURL $ prepare str of
      Right url -> urlExpr url
      Left e -> error e
  , quotePat = pat
  , quoteType = type_
  , quoteDec = dec
  }
  where
    -- | Remove preceeding and succeeding spacing on lines
    prepare :: String -> TS.Text
    prepare str = str
      & lines
      & map (TS.strip . TS.pack)
      & filter (not . TS.null)
      & mconcat

    urlExpr :: URL -> Q Exp
    urlExpr url = let
      t = litE . stringL . TS.unpack
      i = litE . integerL . fromIntegral
      proto' = url^.proto.coerced.to t
      host' = case url^.host of
        IP4 a b c d -> [e| IP4 $(i a) $(i b) $(i c) $(i d) |]
        Domain d -> [e| Domain $(t d) |]
      port' = url^.port.coerced.to (i :: Word16 -> ExpQ)
      path' = [e| Path $(url^.path.segments & map t & listE) |]
      params' = [e| Params $(let
           params' = url^.params.coerced :: [(TS.Text, Maybe TS.Text)]
        in params' & map paramE & listE) |]
        where
          paramE (k, mv) = [e| Param $(tupE [t k, valueE mv]) |]
          valueE mv = case mv of
            Just a -> [e| Just $(t a) |]
            Nothing -> [e| Nothing |]
      fragment' = [e| Fragment $(case url^.fragment of Fragment t' -> t t') |]
      in [e| URL $(proto') (Authority Nothing $(host') $(port'))
                 $(path') $(params') $(fragment') |]

    pat :: String -> Q Pat
    pat = undefined

    type_ :: String -> Q Type
    type_ _ = undefined

    dec :: String -> Q [Dec]
    dec _ = return []
