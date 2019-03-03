{-# LANGUAGE PackageImports #-}

module Imports.Support.CLI
( options
, Options(..)
) where

import qualified "base"                      Data.Foldable as F
import           "optparse-applicative"      Options.Applicative
import           Types

data Options =
    PrintOpt {_printOpt_dir :: FilePath}
    | ViewOpt {_viewOpt_dir :: FilePath}
    | ExecuteOpt {_executeOpt_dir :: FilePath}

options :: ParserInfo Options
options = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Run Import Support"
    )


optionsParser :: Parser Options
optionsParser = subparsersWithHelp
    [ ("print"   , "Prints execution plan for path" , PrintOpt <$> packageOpt)
    , ("view"    , "Generates tempFiles with prefix " ++ show prefixHeader ++ "in packages' dirs" , ViewOpt <$> packageOpt)
    , ("execute" , "Executes plan for path, use this option with Caution", ExecuteOpt <$> packageOpt)
    ]
packageOpt :: Parser FilePath
packageOpt =
   strOption
    ( long "filepath"
   <> short 'd'
   <> showDefault
   <> help "runs imports support on a package provided in path or directory"
    )


cmdWithHelp :: String
            -> String
            -> Parser a
            -> Mod CommandFields a
cmdWithHelp cmdName desc opts =
    command
        cmdName $
        info
            (helper <*> opts)
            (fullDesc <> progDesc desc)

subparsersWithHelp :: [(String,String, Parser a)] -> Parser a
subparsersWithHelp = subparser . F.foldMap (\(x, y, z) -> cmdWithHelp x y z)
