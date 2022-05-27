{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{- |
  TH-generate @ToExpr@ and @HasField@ instances data types.
-}
module JS.TH where

import Prelude as P
import Data.Char
import Data.List
import Control.Lens hiding (Empty)
import Data.Maybe

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import JS.DSL


data DataDecl
  -- | No data constructors: @data Void@
  = Empty

  -- | @data T = A Field1 Field2 | B ...@
  | RegularData Name [(Name, Con)]

  -- | @data T = A | B ...@
  | Enum Name [Con]

  -- | @data R = R { a :: Int, b :: String, d :: Double } @
  | SingleRecord Name Name [(Name, Type)]

  -- | Mixed record and non-record data constructors
  -- @data D = A { a :: Int} | B String Double | C@
  | MultiMixed

  -- | Data family instance: @data instance Family Int = FamilyInt Something@
  | DataFamilyInstance Type Name [(Name, Type)]

-- | Deriev ToExpr instance for data type
deriveToExpr :: Name -> Q [Dec]
deriveToExpr name = name2dataDecl name >>= \case
  Enum typeName _ -> [d| instance {-# OVERLAPPING #-} ToExpr $(conT typeName) where lit v = lit (show v) |]
  SingleRecord typeName conName fieldNamesTypes ->
    singleRecordToExpr (conT typeName) conName fieldNamesTypes
  DataFamilyInstance type_ conName fieldNamesTypes ->
    singleRecordToExpr (pure type_) conName fieldNamesTypes
  MultiMixed -> fail "Multi-mixed data types not implemented"
  RegularData _ _ -> fail "RegularData not implemented"
  Empty -> fail "Type has no data constructors"

-- | Derieve fieldaccessors for data type
makeExprFields :: Name -> Q [Dec]
makeExprFields name = name2dataDecl name >>= \case
  SingleRecord typeName conName fieldNamesTypes ->
    singleRecordHasField (conT typeName) conName fieldNamesTypes
  DataFamilyInstance type_ conName fieldNamesTypes ->
    singleRecordHasField (pure type_) conName fieldNamesTypes
  _ -> fail "makeExprFields: not implemented"

-- | Reify name to a type and data constructor pairs
name2dataDecl :: Name -> Q DataDecl
name2dataDecl name = reify name >>= \info -> case info of
  TyConI dec -> constructors dec
  DataConI conName _ parentName -> do
    parentInfo <- reify parentName
    case parentInfo of
      FamilyI (DataFamilyD _ _ _) (visibleInstances :: [Dec]) -> do
        let
          singleConInstances = filter isSingleDICon visibleInstances
          maybeDec :: Maybe Dec
          maybeDec = find ((nameBase conName ==) . nameBase . dcName . getCon) singleConInstances
        case maybeDec of
          Just di@ (DataInstD _ _ _ _ [con] _) -> do
            let (_ {- conName -- same as the one in scope -}, fieldNamesTypes) = record con
            return $ DataFamilyInstance (mkAppliedTyped di) conName fieldNamesTypes
          _ -> fail "here here here"
      _ -> fail "name2dataDecl unhandled case"
    where
      getCon a = case a of
        DataInstD _ _ _ _ [con] _ -> con
        _ -> error "getCon"
      isSingleDICon a = case a of
        DataInstD _ _ _ _ [_] _ -> True
        _ -> False
#if MIN_VERSION_template_haskell(2,15,0)
      mkAppliedTyped :: Dec -> Type
      mkAppliedTyped (DataInstD _ typeFirst_ (type_ :: Type) _ [_] _) = appliedTyped
        where
          tyVarBndrName :: TyVarBndr -> Name
          tyVarBndrName tvb = case tvb of
            PlainTV name -> name
            KindedTV name _ -> name
          appliedTyped = case typeFirst_ of
            Just as -> foldl AppT type_ $ map (ConT . tyVarBndrName) as
            _ -> type_
#else
      mkAppliedTyped :: Dec -> Type
      mkAppliedTyped (DataInstD _ typeFirst (types :: [Type]) _ _ _) = appliedTyped
        where appliedTyped = foldl AppT (ConT typeFirst) types :: Type
#endif
      mkAppliedTyped _ = error "name2dataDecl: the impossible happened -- the argument is always `DataInstD`"

  _ -> fail "name2dataDecl: Not implemented"
  where

  vbt2vt :: VarBangType -> (Name, Type)
  vbt2vt vbt = (vbt^._1, vbt^._3)

  record :: Con -> (Name, [(Name, Type)])
  record dc = case dc of
    RecC conName vbts -> (conName, map vbt2vt vbts)
    _ -> error "record"

  -- | Extract type name and data constructors with names
  constructors :: Dec -> Q DataDecl
  constructors dec = case dec of
    DataD    _ typeName _ _ [RecC conName vbts] _ ->
      return $ SingleRecord typeName conName $ map vbt2vt vbts
    DataD    _ typeName _ _ [RecGadtC (conName : _) vbts _] _ ->
      return $ SingleRecord typeName conName $ map vbt2vt vbts

    DataD    _ typeName _ _ dataCons _
      | Just _ <- fieldlessDCs dataCons -> return $ Enum typeName dataCons
      | otherwise -> return Empty

    NewtypeD _ _ _ _ _ _ -> do
      return Empty
    DataInstD _ _ _ _ _ _ -> do
      return Empty
    _ -> do
      return Empty

-- * Splice generators

singleRecordHasField :: TypeQ -> Name -> [(Name, Type)] -> Q [Dec]
singleRecordHasField type_ conName fieldNamesTypes = do
  let namesTypes = lensFields (nameBase conName) $ map (_1 %~ nameBase) fieldNamesTypes :: [(String, Type)]
  P.concat <$> mapM (mkHasField type_) namesTypes

singleRecordToExpr :: TypeQ -> Name -> [(Name, Type)] -> Q [Dec]
singleRecordToExpr type_ conName fieldNamesTypes = do
  let namesTypes = lensFields (nameBase conName) $ map (_1 %~ nameBase) fieldNamesTypes :: [(String, Type)]
  mkToExpr type_ namesTypes

mkToExpr :: TypeQ -> [(String, Type)] -> Q [Dec]
mkToExpr mainType namesTypes = case namesTypes of
  nameType : rest -> [d|
    instance {-# OVERLAPPING #-} ToExpr $(mainType) where
      lit v = lit $(listE $ toTup nameType : map toTup rest :: ExpQ)
    |]
  _ -> fail "mkToExpr: "
  where
  toTup :: (String, Type) -> ExpQ
  toTup (name, type_) = [e| ($(stringE name), lit (v ^. $(varE $ mkName name) :: $(pure type_)) ) |]


-- | Make HasField instance
mkHasField :: TypeQ -> (String, Type) -> DecsQ
mkHasField mainType (name, type_) = let
    cls = mkName $ "Has" <> (name & ix 0 %~ toUpper)
    src = [t| Expr $(mainType) |] :: TypeQ
    dest = [t| Expr $(pure type_) |] :: TypeQ
  in do
  (d : _) <- [d| $(varP $ mkName name) = \f v -> fmap (\newVal -> setAttr name newVal v) (f $ v !. name) |] :: DecsQ
  pure <$> instanceD (pure []) (appT (appT (conT cls) src) dest) [pure d]

-- * Helpers

dcName :: Con -> Name
dcName dc = case dc of
  NormalC name _ -> name
  RecC name _ -> name
  InfixC _ name _ -> name
  ForallC _ _ con -> dcName con
  GadtC [name] _ _ -> name
  RecGadtC [name] _ _ -> name
  _ -> error "dcName"


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

 -- * Helpers

-- | Replacement for lens' @makeFields@ which for data family
-- instances uses the data constructor as the removable prefix to
-- create field names.
makeFields2 :: Name -> DecsQ
makeFields2 name = do
  info <- reify name
  case info of
    DataConI conName _ parentName -> do
      parentInfo <- reify parentName
      case parentInfo of
        FamilyI (DataFamilyD _ _ _) (_ :: [Dec]) -> do
          let
            newFieldNamer _ = oldFieldNamer conName
            newRules = camelCaseFields & lensField .~ newFieldNamer
          makeLensesWith newRules name
        _ -> default_
    _ -> default_
  where
    default_ = makeLenses name
    oldFieldNamer :: FieldNamer -- i.e  Name -> [Name] -> Name -> [DefName]
    oldFieldNamer = camelCaseFields^.lensField
