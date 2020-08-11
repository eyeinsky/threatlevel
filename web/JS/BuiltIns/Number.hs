module JS.BuiltIns.Number where

import JS.DSL

parseFloat a = call1 (ex "parseFloat") a

nearestInt n = call1 (math "round") n

parseInt a = call1 (ex "parseInt") a

min a b = ternary (a .< b) a b

max a b = ternary (a .> b) a b

floor' = call1 (math "floor")

ceiling' = call1 (math "ceil")
