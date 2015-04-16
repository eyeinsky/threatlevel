module JS_Types where

data String
data Array
data Number
data Bool
data Object

class BinOp a where
instance BinOp String
instance BinOp Number

instance BinOp Bool
