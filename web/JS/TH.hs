module JS.TH (deriveToExpr) where

import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Data.Char
import Data.List

import X.Prelude

import JS.Syntax as J hiding (getName, Name)
import JS.DSL as J hiding (func)



-- * Data type to lens-like object

deriveToExpr :: Name -> Q [Dec]
deriveToExpr name' = do
  -- get data type name
  info <- reify name'
  -- get field names
  -- drop type name prefix, lower-case the first letter
  let Just (tyName, cons) = case info of TyConI dec -> constructors dec
      namesTypes = map (fieldNames ^ lensFields) cons :: [[(String, Type)]]
      names = map fst $ head namesTypes :: [String]

      toTup :: String -> ExpQ
      toTup name = [e| ($(stringE name), lit (v ^. $(varE $ mkName name)) ) |]

      -- instance ToExpr Type where lit v = [("field", lit (v^.field))]
      toExpr :: DecsQ
      toExpr = [d|
        instance {-# OVERLAPPING #-} ToExpr $(conT tyName) where
          lit v = lit $(listE $ map toTup names) -- names
        |]

      hasName :: (String, Type) -> DecsQ
      hasName (name, type_) = let
          cls = mkName $ "Has" <> (name & ix 0 %~ toUpper)
          src = [t| Expr $(conT tyName) |] :: TypeQ
          dest = [t| Expr $(pure type_) |] :: TypeQ
        in do
        (d : _) <- [d| $(varP $ mkName name) = \f v -> fmap (\newVal -> setAttr name newVal v) (f $ v !. name) |] :: DecsQ
        pure <$> instanceD (pure []) (appT (appT (conT cls) src) dest) [pure d]
      printMe = names

  -- runIO $ print ("here", cons)
  -- let lenses = [hasName $ head $ head namesTypes]
  let lenses = map hasName (head namesTypes)
  concat <$> sequence (toExpr : lenses)

-- | Constructors

constructors :: Dec -> Maybe (Name, [Con])
constructors dec = case dec of
  DataD    _ name _ _ ds _ -> Just (name, ds)
  NewtypeD _ name _ _ d _ -> Just (name, [d])
  _ -> Nothing

-- | Con -> ("Constr", ["ConstrField1", "ConstrField2"])
fieldNames :: Con -> (String, [(String, Type)])
fieldNames c = case c of
  RecC n x -> (nameBase n, f x)
  RecGadtC (n : _) x _ -> (nameBase n, f x)
  _ -> error "hej hej"
  where
    f = map (\(v, _, t) -> (nameBase v, t))

-- | ("Constr", ["ConstrField1", "ConstrField2"]) -> ["field1", "field2"]
lensFields :: (String, [(String, Type)]) -> [(String, Type)]
lensFields (n, li) = li''
  where
    firstToLower = ix 0 %~ toLower :: String -> String
    n' = firstToLower n
    li' = map (first (stripPrefix n')) li :: [(Maybe String, Type)]
    li'' = map (first (ix 0 %~ toLower)) $ map (first fromJust) $ filter (fst ^ isJust) li'
