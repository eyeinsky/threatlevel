module JS_Derive (myDeriveJS) where

import Prelude2
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

import qualified JS as J

type St = Prelude2.String
data E = E Int
data F = F { f :: Int }
data A = A Int St
data B = B1 Int St | B2 Int St
data C = C { cint :: Int, cstr :: St }
data D = D1 { d1int :: Int, d1str :: St } | D2 { d2int :: Int, d2str :: St }

myDeriveJS opts t = do
   ds <- deriveJSON opts t
   -- ds <- return []
   x <- deriveForType t
   return (ds <> x)
   

deriveForType
   :: Name     -- ^ Name of the type
   -> Q [Dec]  -- ^ List of js functions to create each data constructor
deriveForType tyName = go =<< reify tyName
   where
      go con = case con of
         TyConI x -> case x of
            DataD    _ _ _ (dcs::[Con]) _
               -> f123 dcs (fmap pure . singleCon) (mapM multiCon)
            NewtypeD _ _ _ dc  _
               -> pure <$> singleCon dc
            x -> error $ show x
         x -> error "need a TyConI!"

      error x = do
         reportError $ x 
         return u
      
f123 xs f g = case xs of
   _ : _ : _ -> g xs
   x : []    -> f x



-- * Creators by type


-- | >1 data constructors (data T = A .. | B ..)
multiCon :: Con {-^ Data constructor -} -> Q Dec {-^ The respective function -}
multiCon dc = case dc of
   RecC name vst   -> func dc [recClause (Just name) False $ map vstName vst ]
   NormalC name st -> func dc [ dtClause (Just name) True    (length st) ]
   where f n xs = (tupE . map stringE $ ["tag", nameBase n]) : xs
-- multirecord: no contents, multi-normal: tag with contents


-- | Single data constructor (data T = A ..)
singleCon :: Con -> Q Dec
singleCon dc = case dc of
   RecC    _ vst -> func dc [recClause Nothing True $ map vstName vst ]
   NormalC _  st -> f123 st
      (\x -> newName "a" >>= \ n -> func dc [clause [varP n] (normalB $ varE n) []] )
      (\st -> func dc [dtClause  Nothing False (length st)])


-- * Helpers
   

-- mkClause :: Maybe ExpQ -> [String] -> ClauseQ
recClause mTag ifcontents xs = do
   names <- mapM newName xs
   lhs names (mkBody mTag ifcontents $ zipWith mkTup xs names) []
dtClause mTag ifcontents n = do
   names <- replicateM n (newName "a")
   lhs names (mkBody mTag ifcontents $ map varE names) []

lhs names body = clause (map varP names) body

mkBody mb bcont li = normalB $ listE $ maybe li
   (\name -> let tag = mkTag name
      in if bcont
       then [ tag, tupE [stringE "contents", listE li ] ]
       else tag : li)
   mb


mkTag name = tupE . map stringE $ ["tag", nameBase name]

mkTup :: String -> Name -> ExpQ
mkTup s n = tupE [litE $ stringL s, varE n]

vstName (a,_,_) = nameBase a

func x y = funD (prefix $ getName x) y

getName c = case c of 
   RecC name _ -> name
   NormalC name _ -> name

prefix = mkName . ("js" <>) . nameBase
