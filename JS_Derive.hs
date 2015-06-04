module JS_Derive (myDeriveJS) where

import Prelude2
--import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

import qualified JS as J


myDeriveJS mbOpts typeName = do
   aesonToFrom <- maybe (const $ return []) deriveJSON mbOpts $ typeName
   js <- deriveForType typeName
   runIO $ putStrLn $ pprint js
   return (aesonToFrom <> js)
   

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
   _ -> error "siin 1"




-- * Creators by type

-- | >1 data constructors (data T = A .. | B ..)
multiCon :: Con {-^ Data constructor -} -> Q Dec {-^ The respective function -}
multiCon dc = do
   -- let argt = [t| J.Expr $(newType "a") |] 
   case dc of
      RecC name vst   -> funD fn [recClause (Just name) False $ map vstName vst ]
      NormalC name st -> case st of
         [] -> let empty = [| J.ULit (J.ULArray []) |]
            in funD fn [ clause [] (normalB 
                 [| [ $(mkTag name), $(mkContents empty) ] |]) [] ]
         _ -> funD fn [ dtClause (Just name) True    (length st) ]
   where fn = fname dc 



-- | Single data constructor (data T = A ..)
singleCon :: Con -> Q Dec
singleCon dc = case dc of
   RecC    _ vst -> func dc [recClause Nothing True $ map vstName vst ]
   NormalC _  st -> f123 st
      (\x -> newName "a" >>= \ n -> func dc [clause [varP n] (normalB $ varE n) []] )
      -- ^ single-data single-value constructor (data A = A Int)
      (\st -> func dc [dtClause  Nothing False (length st)])
      -- ^ single-data multi-value constructor ()



-- * Helpers
   
recClause :: Maybe Name -> Bool -> [String] -> ClauseQ
recClause mTag ifcontents xs = do
   names <- mapM newName xs
   lhs names (mkBody mTag ifcontents $ zipWith mkTup xs names)
  where
      mkTup :: String -> Name -> ExpQ
      mkTup s n = [| ($k, $(nameCast n)) |]
         where k = stringE s
               -- v = varE n


dtClause :: Maybe Name -> Bool -> Int -> ClauseQ
dtClause mTag ifcontents n = do
   names <- replicateM n (newName "a")
   lhs names (mkBody mTag ifcontents $ nameCast<$>names)

lhs names body = do
   t <- newType "arg"
   -- let f = flip sigP (pure t) . varP -- func arg scoped types
   let f = varP
   clause (map f names) body []
         

mkBody :: Maybe Name -> Bool -> [ExpQ] -> Q Body
mkBody mb inContents li = do
   et <- newType "elemType"
   let sig = id -- [| $li :: [J.Expr et] |] 
   normalB $ appE [|J.ulit|] $ sig $ listE $ maybe li
      (\name -> let tag = mkTag name
         in if inContents
          then let li' = [| J.ulit $(listE li) |]
            in [ tag, mkContents li' ]
          else tag : li)
      mb


-- * JS related helpers

mkTag name = [| ("tag"::String, J.ulit $(strName name)) |]
mkContents xs = [| ("contents" :: String, $xs) |]

arrExpr e = [| $e :: [J.Expr a] |]
objExpr e = [| $e :: [(a, J.Expr b)] |]

nameCast :: Name {-^ of type Expr a -} -> ExpQ
nameCast name = [| J.Cast $(varE name) |]

prefix = mkName . ("js" <>) . nameBase
func x = funD (prefix $ getName x)


-- * General purpose helpers

strName = stringE . nameBase

vstName (a,_,_) = nameBase a

fname = prefix . getName

getName c = case c of 
   RecC name _ -> name
   NormalC name _ -> name

newType p = VarT <$> newName p

constType n t = go n
   where 
      go n = AppT (AppT ArrowT t) . go (n - 1)
