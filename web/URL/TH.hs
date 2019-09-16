module URL.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import X.Prelude hiding (un)

import qualified Data.Text as TS
import Data.List (intercalate)
import Data.Either
import URL

url = QuasiQuoter
  { quoteExp = \str -> case URL.parseURL $ TS.pack $ filter' str of
      Right url -> urlExpr url
      Left e -> error e
  , quotePat = pat
  , quoteType = type_
  , quoteDec = dec
  }
  where
    filter' = filter (\c -> c /= ' ' && c /= '\n')

    urlExpr :: URL -> Q Exp
    urlExpr url = let
      t = litE . stringL . TS.unpack
      i = litE . integerL . fromIntegral
      proto' = url^.proto.un.to t
      host' = case url^.host of
        IP4 a b c d -> [e| IP4 $(i a) $(i b) $(i c) $(i d) |]
        Domain d -> [e| Domain $(t d) |]
      port' = url^.port.un.to i
      path' = [e| Path $(url^.path.segments & map t & listE) |]
      params' = [e| Params $(url^.params.un & map paramE & listE) |]
        where
          paramE (k, mv) = tupE [t k, valueE mv]
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
