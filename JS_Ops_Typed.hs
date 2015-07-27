module JS_Ops_Typed

import Prelude2
import JS_Types as JT
-- ** Operators

-- typed
e1 .==  e2 = bop JT.eq e1 e2
e1 .=== e2 = bop JT.eeq e1 e2
e1 .!=  e2 = bop JT.neq e1 e2
e1 .!== e2 = bop JT.neeq e1 e2

e1 .&& e2 = bop JT.and e1 e2
e1 .|| e2 = bop JT.or e1 e2

e1 .>  e2 = bop JT.gt e1 e2
e1 .<  e2 = bop JT.lt e1 e2
e1 .>= e2 = bop JT.gte e1 e2
e1 .<= e2 = bop JT.lte e1 e2

e1 .+ e2 = bop JT.plus e1 e2
e1 .- e2 = bop JT.minus e1 e2
e1 .* e2 = bop JT.mult e1 e2
e1 ./ e2 = bop JT.div e1 e2
bop p a b = BOp $ BOE p a b


