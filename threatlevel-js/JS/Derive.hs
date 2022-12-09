module JS.Derive (deriveJS) where

import Common.Prelude as P
import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

import JS.Syntax as J hiding (Name)
import JS.DSL as J

deriveJS
  :: Maybe Options
  -- ^ Derive ToJSON/FromJSON with these options. Doesn't derive when Nothing.
  -> Name
  -- ^ Name of the type being derived
  -> Q [Dec]
deriveJS mbAesonOpts typeName = do
  let li = [ConT typeName]
  _ <- reifyInstances ''ToJSON li
  _ <- reifyInstances ''FromJSON li
  aesonToFrom <- maybe (\_ -> return []) deriveJSON mbAesonOpts $ typeName
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
         DataD    _ _ _ _ (dcs::[Con]) _
            -> f123 dcs (fmap pure . singleCon) (mapM multiCon)
         NewtypeD _ _ _ _ dc _
            -> pure <$> singleCon dc
         x -> error $ show x
      _ -> error "need a TyConI!"

    error x = do
      reportError $ x
      return todo

    f123 xs f g = case xs of
      _ : _ : _ -> g xs
      x : []    -> f x
      _ -> error "siin 1"




-- * Creators by type

-- | >1 data constructors (data T = A .. | B ..)
multiCon :: Con {-^ Data constructor -} -> Q Dec {-^ The respective function -}
multiCon dc = case dc of
  RecC name vst   -> funD fn [recClause (Just name) False $ map vstName vst ]
  NormalC name bt -> nameBangType name bt
  GadtC (name : _) bt _ -> nameBangType name bt
  _ -> todo
  where
    fn = fname dc
    nameBangType name st = case st of
      _ : _ : _ -> funD fn [ dtClause (Just name) True    (length st) ]
      _ : _ -> do
        arg <- newName "a"
        funD fn [ clause [varP arg] (normalB
          [| J.lit [ $(mkTag name), $(mkContents $ varE arg) ] |]) [] ]
      _ -> let empty = [| J.Lit (J.Array []) |]
        in funD fn [ clause [] (normalB
             [| J.lit [ $(mkTag name), $(mkContents empty) ] |]) [] ]



-- | Single data constructor (data T = A ..)
singleCon :: Con -> Q Dec
singleCon dc = case dc of
  RecC    _ vst -> func dc [recClause Nothing True $ map vstName vst ]
  NormalC _  st -> case st of
    _ : _ : _ -> func dc [dtClause  Nothing False (length st)]
    -- ^ single-data multi-value constructor ()
    _ : _ -> newName "a" >>= \ n -> func dc [clause [varP n] (normalB $ varE n) []]
    _ -> todo
    -- ^ single-data single-value constructor (data A = A Int)
  _ -> todo

-- * Helpers

recClause :: Maybe Name -> Bool -> [String] -> ClauseQ
recClause mTag ifcontents xs = do
  names <- mapM newName xs
  lhs names (mkBody mTag ifcontents $ zipWith mkTup xs names)
  where
    mkTup :: String -> Name -> ExpQ
    mkTup s n = let
        k = stringE s
      in [| ($k, $(nameCast n)) |]

dtClause :: Maybe Name -> Bool -> Int -> ClauseQ
dtClause mTag ifcontents n = do
  names <- replicateM n (newName "a")
  lhs names (mkBody mTag ifcontents $ nameCast<$>names)

lhs :: [Name] -> BodyQ -> ClauseQ
lhs names body = clause (map varP names) body []


mkBody :: Maybe Name -> Bool -> [ExpQ] -> Q Body
mkBody mb inContents li = normalB $ appE [|J.lit|] $ listE $ maybe li
  (\name -> let tag = mkTag name
    in if inContents
    then let li' = [| J.lit $(listE li) |]
      in [ tag, mkContents li' ]
    else tag : li)
  mb


-- * JS related helpers

mkTag :: Name -> ExpQ
mkTag name = [| ("tag"::String, J.lit $(strName name)) |]

mkContents :: ExpQ -> ExpQ
mkContents xs = [| ("contents" :: String, $xs) |]

nameCast :: Name {-^ of type Expr a -} -> ExpQ
nameCast name = [| J.Cast $(varE name) |]

prefix :: Name -> Name
prefix = mkName . ("js" <>) . nameBase

func :: Con -> [ClauseQ] -> DecQ
func x = funD (prefix $ getName x)

-- * General purpose helpers

strName :: Name -> ExpQ
strName = stringE . nameBase

vstName :: (Name, b, c) -> String
vstName (a,_,_) = nameBase a

fname :: Con -> Name
fname = prefix . getName

getName :: Con -> Name
getName c = case c of
   RecC name _ -> name
   NormalC name _ -> name
   GadtC (name : _) _ _ -> name
   _ -> todo
