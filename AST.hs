
module AST where


type Code = [Statement]

data Statement
   = FuncDef String FormalArgs Code
   | VarDef Name [Name] Expr -- a = [b = c =] expr
   | FuncCall Expr
   | Expr Expr
   | For Statement Expr Statement Code
   | ForIn Name Expr Code
   | TryCatch Code Code
data Expr
   = EName Name         -- name
   | Arr  Expr Expr     -- expr [ expr ]
   | Attr Expr String   -- expr.name
   | Literal Literal    -- lit
   | Op OpExpr
data OpExpr
   = OpBinary BOp Expr Expr
   | OpUnary UOp Expr
data UOp = UMinus | UPlus | TypeOf
data BOp = BMinus | BPlus | Mult | Div
data Literal
   = String String
   | Number String
   | Object [PropDef]
data PropDef = PropDef (Either Name Literal) Expr -- a : expr
data FormalArgs = FA [String]
data Name = Name String


print :: Code -> String
print = undefined


-- hasOwnProperty
