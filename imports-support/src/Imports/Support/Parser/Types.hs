{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imports.Support.Parser.Types where

import                                Data.Typeable
import           "base"                Data.Data

data Qualified = QualDef
               | QualOnlyAs ModuleName
               | QualAs ModuleName
    deriving (Show,Data,Typeable)

type ModuleName = [String]
data ImportStmt =
    ImportStmt
        { _importStmtQualOnly_pkgImport  :: Maybe String
        , _importStmtQualOnly_moduleName :: ModuleName
        , _importStmtQualOnly_importExtra :: Maybe ImportExtra
        , _importStmtQualOnly_qualified :: Qualified
        }

        deriving (Show,Data,Typeable)

data ImportExtra = HidingList [String]
                 | ImportList [String]
                 | InstancesOnly
    deriving (Show,Data,Typeable)