{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import           "base"                Data.Typeable
import           "base"                Data.Data
import           "base"                Data.List
import           "filepath"            System.FilePath

data Action = PrintPlan
            | Execute

data SearchOpts = SearchOpts FilePath Action

actionFromPrint :: Bool -> Action
actionFromPrint True = PrintPlan
actionFromPrint _    = Execute

data WorkTree = Package Annotation FilePath
              | Directory Annotation FilePath [WorkTree]
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

prefixHeader :: String
prefixHeader = "._IS_"

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