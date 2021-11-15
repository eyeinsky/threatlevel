module JS.BuiltIns.Number where

import JS.DSL

parseFloat :: Expr b -> Expr c
parseFloat a = call1 (ex "parseFloat") a

nearestInt :: Expr b -> Expr c
nearestInt n = call1 (math "round") n

parseInt :: Expr b -> Expr c
parseInt a = call1 (ex "parseInt") a

min :: Expr a -> Expr a -> Expr a
min a b = ternary (a .< b) a b

max :: Expr a -> Expr a -> Expr a
max a b = ternary (a .> b) a b

floor' :: Expr b -> Expr c
floor' = call1 (math "floor")

ceiling' :: Expr b -> Expr c
ceiling' = call1 (math "ceil")
