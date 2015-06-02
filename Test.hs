
import Prelude2
import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH
import JS_Derive


type St = Prelude2.String


data D1 = D1 Int
data D2 = D2 Int St
data D3 = D3 Int St | D3' Int St

data R1 = R1 { r1 :: Int }
data R2 = R2 { r2int :: Int, r2str  :: St }
data R3 = R3 { r3int :: Int, r3str :: St } | R3' { r3'int :: Int, r3'str :: St }


$(myDeriveJS defaultOptions ''D1)
$(myDeriveJS defaultOptions ''D2)
$(myDeriveJS defaultOptions ''D3)
$(myDeriveJS defaultOptions ''R1)
$(myDeriveJS defaultOptions ''R2)
$(myDeriveJS defaultOptions ''R3)
