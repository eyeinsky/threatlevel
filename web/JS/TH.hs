module JS.TH (deriveToExpr) where

import Language.Haskell.TH
import Data.Char
import Data.List

import X.Prelude

import JS.Syntax as J hiding (getName, Name)
import JS.DSL as J hiding (func)
import JS.Lib as J



-- * Data type to lens-like object

dcNamesTypes :: [Con] -> [[(String, Type)]]
dcNamesTypes dcs = map (fieldNames ^ lensFields) dcs :: [[(String, Type)]]

deriveToExpr :: Name -> Q [Dec]
deriveToExpr name' = do

  Just (typeName, dataCons) <- reify name' <&> \case
    TyConI dec -> constructors dec
    _ -> Nothing

  let namesTypes = dcNamesTypes dataCons
      names = map fst $ head namesTypes :: [String]

      toTup :: String -> ExpQ
      toTup name = [e| ($(stringE name), lit (v ^. $(varE $ mkName name)) ) |]

      toExpr :: DecsQ
      toExpr = [d|
        instance {-# OVERLAPPING #-} ToExpr $(conT tyName) where
          lit v = lit $(listE $ map toTup names) -- names
        |]

      hasName :: (String, Type) -> DecsQ
      hasName (name, type_) = let
          cls = mkName $ "Has" <> (name & ix 0 %~ toUpper)
          src = [t| Expr $(conT typeName) |] :: TypeQ
          dest = [t| Expr $(pure type_) |] :: TypeQ
        in do
        (d : _) <- [d| $(varP $ mkName name) = \f v -> fmap (\newVal -> J.setAttr name newVal v) (f $ v !. name) |] :: DecsQ
        pure <$> instanceD (pure []) (appT (appT (conT cls) src) dest) [pure d]

  let lenses = map hasName (head namesTypes)
  concat <$> sequence (toExpr : lenses)

-- | Extract data constructors from type declaration.
constructors :: Dec -> Maybe (Name, [Con])
constructors dec = case dec of
  DataD    _ name _ _ dataCons _ -> Just (name, dataCons)
  NewtypeD _ name _ _ dataCon _ -> Just (name, [dataCon])
  _ -> Nothing

-- | Con -> ("Constr", ["ConstrField1", "ConstrField2"])
fieldNames :: Con -> (String, [(String, Type)])
fieldNames dataCon = case dataCon of
  _ -> error "hej hej"
  RecC name varBangTypes -> (nameBase name, nameType varBangTypes)
  RecGadtC (name : _) varBangTypes _ -> (nameBase name, nameType varBangTypes)
  where
    nameType = map (\(varName, _, type_) -> (nameBase varName, type_))

{- | Use constructor name to strip its prefix from fields,
     return just the unprefixed field names.

   ("Constr", ["ConstrField1", "ConstrField2"]) -> ["field1", "field2"]
-}
lensFields :: (String, [(String, Type)]) -> [(String, Type)]
lensFields (conName, fieldNames) = go fieldNames
  where
    lower = ix 0 %~ toLower :: String -> String
    strip = stripPrefix (lower conName)

    go xs = case xs of
      (prefixed, type_) : rest -> case strip prefixed of
        Just fieldName -> (lower fieldName, type_) : go rest
        _ -> go rest
      _ -> []
