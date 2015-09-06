module JS_Ops_Untyped where

import Prelude2 hiding ((.>))
import JS_Types
import JS_Syntax
-- ** Operators

e1 .==  e2 = Op $ OpBinary   Eq e1 e2
e1 .=== e2 = Op $ OpBinary  EEq e1 e2
e1 .!=  e2 = Op $ OpBinary  NEq e1 e2
e1 .!== e2 = Op $ OpBinary NEEq e1 e2

e1 .&& e2 = Op $ OpBinary And e1 e2
e1 .|| e2 = Op $ OpBinary Or e1 e2

e1 .<  e2  = Op $ OpBinary Lt e1 e2
e1 .>  e2  = Op $ OpBinary Gt e1 e2
e1 .<= e2 = Op $ OpBinary LEt e1 e2
e1 .>= e2 = Op $ OpBinary GEt e1 e2

e1 .+ e2 = Op $ OpBinary Plus e1 e2
e1 .- e2 = Op $ OpBinary Minus e1 e2
e1 .* e2 = Op $ OpBinary Mult e1 e2
e1 ./ e2 = Op $ OpBinary Div e1 e2
