{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import           "base"                Data.Typeable
import           "base"                Data.Data

data Action = PrintDir
            | Modify

data SearchOpts = SearchOpts FilePath Action

actionFromPrint True = PrintDir
actionFromPrint _    = Modify

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
