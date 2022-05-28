{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Apis.Fetch where

import Common.Prelude
import URL
import JS

instance ToExpr URL.URL where
  lit = renderURL ^ lit

type Opts a b = (IsString a, IsString b, ToExpr [(a, b)])
fetch :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetch url extra = call (ex "fetch") [ url, lit extra ]

fetchMethod
  :: Opts a b => b -> Expr URL -> [(a, b)] -> Expr c
fetchMethod method url extra = fetch url opts
  where
    opts =  [("method", method)] <> extra

fetchPost :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetchPost = fetchMethod "POST"

fetchPut :: Opts a b => Expr URL -> [(a, b)] -> Expr c
fetchPut = fetchMethod "PUT"

jsonPayload :: Expr a -> [(String, Expr String)]
jsonPayload data_ =
  [ ("body", toJSON data_)
  , ("headers", lit
      [(lit "Content-Type", "application/json")])
  ]

jsonBody :: Expr a -> [(String, Expr String)]
jsonBody = jsonPayload
