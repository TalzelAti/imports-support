{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import                                Data.Typeable
import           "base"                Data.Data
import           "base"                Data.List
import           "filepath"            System.FilePath

data Action = PrintPlan
            | Execute

data SearchOpts = SearchOpts FilePath Action

actionFromPrint :: Bool -> Action
actionFromPrint True = PrintPlan
actionFromPrint _    = Execute

data WorkTree = Package FilePath Annotation
              | Directory FilePath Annotation [WorkTree]
            deriving (Show, Data, Typeable)

data Annotation = PackageAnnot [FileAnnot]
                | ErrMsg String
                | NoAnnotation
                | PkgsFileMods FilePath [String]
                deriving (Show,Data,Typeable)

data FileAnnot =
    HsFileAnnot
        { faName :: FilePath
        , faHasPackageImports ::Bool
        , faImportsList :: [String]
        }
    | PkgsYaml String
    deriving (Show,Data,Typeable)

-- parser types

data Qualified = QualDef | QualOnlyAs ModuleName | QualAs ModuleName
    deriving (Show)

type ModuleName = [String]
data ImportStmt =
    ImportStmt
        { _importStmtQualOnly_pkgImport  :: Maybe String
        , _importStmtQualOnly_moduleName :: ModuleName
        , _importStmtQualOnly_importExtra :: Maybe ImportExtra
        , _importStmtQualOnly_qualified :: Qualified
        }

        deriving (Show)

data ImportExtra = HidingList [String] | ImportList [String] | InstancesOnly
    deriving (Show)

-- utils

prefixHeader :: String
prefixHeader = "_IS_tmp_file_"

addTempPrefix :: FilePath -> FilePath
addTempPrefix fp
    | prefixHeader `isPrefixOf` takeFileName fp = fp
    | otherwise =
        replaceFileName fp $ prefixHeader <> takeFileName fp


removeTempPrefix :: FilePath -> FilePath
removeTempPrefix fp
    | prefixHeader `isPrefixOf` takeFileName fp =
        replaceFileName fp $ drop (length prefixHeader) $ takeFileName fp
    | otherwise = fp