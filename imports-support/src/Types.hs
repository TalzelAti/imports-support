{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import                                Data.Typeable
import           "base"                Data.Data
import           "base"                Data.List
import           "filepath"            System.FilePath
import                                Imports.Support.Parser.Types

data Action = PrintPlan
            | Execute

data SearchOpts = SearchOpts FilePath Action

actionFromPrint :: Bool -> Action
actionFromPrint True = PrintPlan
actionFromPrint _    = Execute

data WorkTree = Package FilePath Annotation
              | Directory FilePath Annotation [WorkTree]
            deriving (Show, Data, Typeable)

data Annotation = PackageAnnot
                    { _packageAnnot_filesAnnot :: [FileAnnot]
                    , _packageAnnot_filesUpdate :: [FileUpdate]
                    }
                | ErrMsg { _errMsg_errorMessage :: String }
                | NoAnnotation
                deriving (Show,Data,Typeable)

data FileUpdate = PkgsFileUpdate FilePath [String]
                | HsFileUpdate
                    { hfuName :: FilePath
                    , hfuHasPackageImports ::Bool
                    , hfuImports :: [String]
                    , hfuImports_stmts :: [ImportStmt]
                    , hfuImports_new :: [String]
                    }
                deriving (Show,Data,Typeable)

data FileAnnot =
    HsFileAnnot
       { faName :: FilePath
       , faHasPackageImports ::Bool
       , faImports :: [String]
       , faImportsList :: [ImportStmt]
       }
    | PkgsYaml String
    deriving (Show,Data,Typeable)

-- parser types



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