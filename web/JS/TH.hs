module JS.TH where

import Language.Haskell.TH
import Data.Char
import Data.List

import X.Prelude

import JS.Syntax as J hiding (getName, Name)
import JS.DSL as J hiding (func)
import JS.BuiltIns as J


-- * Data type to lens-like object

-- | Make @ToExpr@ instances for data type
deriveToExpr :: Name -> Q [Dec]
deriveToExpr name = do
  Just (typeName, dcs :: [Con]) <- typeAndDCs name
  case dcs of
    _ : _ : _
      -- | @data T = A | B ...@
      | Just xs <- fieldlessDCs dcs -> [d|
          instance {-# OVERLAPPING #-} ToExpr $(conT typeName) where
            lit v = lit $([e|show v|])
          |]

      | otherwise -> fail "Multi-constructor data types not implemented"

    dc : [] -> doDC typeName dc
    _ -> fail "Type has no data constructors"


doDC :: Name -> Con -> Q [Dec]
doDC typeName dataCon = do
  let namesTypes = uncurry lensFields . fieldNames $ dataCon :: [(String, Type)]
      names = map fst namesTypes :: [String]

      toTup :: String -> ExpQ
      toTup name = [e| ($(stringE name), lit (v ^. $(varE $ mkName name)) ) |]

      toExpr :: DecsQ
      toExpr = [d|
        instance {-# OVERLAPPING #-} ToExpr $(conT typeName) where
          lit v = lit $(case names of
            [] -> [e|show v|]
            _ -> listE $ map toTup names :: ExpQ)
        |]

      hasName :: (String, Type) -> DecsQ
      hasName (name, type_) = let
          cls = mkName $ "Has" <> (name & ix 0 %~ toUpper)
          src = [t| Expr $(conT typeName) |] :: TypeQ
          dest = [t| Expr $(pure type_) |] :: TypeQ
        in do
        (d : _) <- [d| $(varP $ mkName name) = \f v -> fmap (\newVal -> J.setAttr name newVal v) (f $ v !. name) |] :: DecsQ
        pure <$> instanceD (pure []) (appT (appT (conT cls) src) dest) [pure d]

  let lenses = map hasName namesTypes
  X.Prelude.concat <$> sequence (toExpr : lenses)

-- | Reify name to a type and data constructor pairs
typeAndDCs :: Name -> Q (Maybe (Name, [Con]))
typeAndDCs name = reify name >>= \info -> case info of
  TyConI dec -> pure $ constructors dec
  DataConI _ _ parentName -> typeAndDCs parentName
  _ -> do
    pure $ Nothing

-- | Extract data constructors from type declaration.
constructors :: Dec -> Maybe (Name, [Con])
constructors dec = case dec of
  DataD    _ name _ _ dataCons _ -> Just (name, dataCons)
  NewtypeD _ name _ _ dataCon _ -> Just (name, [dataCon])
  _ -> Nothing

-- | Con -> ("Constr", ["constrField1", "constrField2"])
fieldNames :: Con -> (String, [(String, Type)])
fieldNames dataCon = case dataCon of
  RecC name varBangTypes -> (nameBase name, nameType varBangTypes)
  RecGadtC (name : _) varBangTypes _ -> (nameBase name, nameType varBangTypes)
  NormalC name _ -> (nameBase name, [])
  a -> error (show a)
  where
    nameType = map (\(varName, _, type_) -> (nameBase varName, type_))

fieldless :: Con -> Bool
fieldless dc = case dc of
  NormalC _ [] -> True
  _ -> False

fieldlessDCs :: [Con] -> Maybe [String]
fieldlessDCs dcs = if all isJust xs
  then Just $ catMaybes xs
  else Nothing
  where
    xs = map getFieldless dcs

    getFieldless :: Con -> Maybe String
    getFieldless dc = case dc of
      NormalC name [] -> Just (nameBase name)
      _ -> Nothing


-- * Field name helpers

-- | Use constructor name to strip its prefix from fields, return just
-- the unprefixed field names.
lensFields :: String ->  [(String, Type)] -> [(String, Type)]
lensFields conName fieldNames = go fieldNames
  where
    go xs = case xs of
      (prefixed, type_) : rest -> case mkField conName prefixed of
        Just fieldName -> (lowerFirst fieldName, type_) : go rest
        _ -> go rest
      _ -> []

-- | Just strip type constructor name from field, otherwise Nothing
mkField :: String -> String -> Maybe String
mkField type_ field = lowerFirst <$> (mkStripper type_ field)
  where
    mkStripper :: String -> String -> Maybe String
    mkStripper type_ = stripPrefix (lowerFirst type_)

-- | Lower-case the first letter
lowerFirst :: String -> String
lowerFirst str = case str of
  s : tr -> toLower s : tr
  _ -> str
