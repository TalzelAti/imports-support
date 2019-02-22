{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           "directory"           System.Directory
import           "base"                Data.List
import           "base"                Data.Monoid
import           "base"                Control.Monad
import           "filepath"            System.FilePath
import           "optparse-applicative"      Options.Applicative
import           "pretty-simple"       Text.Pretty.Simple


import Debug.Trace

lttrace a b = trace (a ++ ":" ++ show b) b

-- add flag to run on full directories and scan haskell packages in them

someFunc :: IO ()
someFunc = execParser opts
    >>= updatePackageDeps
    where
        opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Run Import Support"
            )
        options :: Parser SearchOpt
        options = packageOpt
              <|> directoryOpt

        packageOpt :: Parser SearchOpt
        packageOpt = Package <$> strOption
            ( long "filepath"
           <> short 'p'
           <> showDefault
           <> help "runs imports support on a package provided in path"
            )
        directoryOpt :: Parser SearchOpt
        directoryOpt = Directory <$> strOption
            ( long "directory"
           <> short 'd'
           <> showDefault
           <> help "runs imports support on all packages in directory (including subdirs)"
            )

    --moduleNames <- getImportsFromFile fp
    --print $ map checkPackageImports fcs
    --print =<< listDirectory "."
    --updatePackageDeps "/home/talz/development/atidot/gapsight/gapsight-server"
    --print =<< listAllFiles "/home/talz/development/atidot/gapsight/gapsight-server"

data SearchOpt = Package FilePath
               | Directory FilePath



-- ensure argument is a dir

-- todo: ignore pack

updatePackageDeps (Package packagePath) = do
    files <- listAllFiles packagePath
    putStrLn $ "files:\n" ++ unlines files
    let haskellFiles = filter (isSuffixOf ".hs") files
        packageYamlFile = head $ filter (isSuffixOf "package.yaml") files
    alldependencies <- mapM getImportsFromFile haskellFiles
    let alldependencies' = nub $ concat alldependencies
    putStrLn $ "all Files Dependencies:\n" ++ unlines alldependencies'
    packageYamlContent <- readFile packageYamlFile
    putStrLn $ "original content:\n" ++ packageYamlContent
    let modifiedContent = modifyPackagesSection alldependencies' packageYamlContent
    putStrLn $ "modified content:\n" ++ modifiedContent
    writeFile packageYamlFile modifiedContent

-- todo: deal with versions
-- todo: tests and appSystem.FilePath
-- todo: literate haskell
-- todo: allow only with package imports
-- ensure package.yaml existence

modifyPackagesSection :: [String] -> String -> String
modifyPackagesSection packages fileContent =
    let unyamlListElem = drop 2
        yamlListElem = (++) "- "
        fileLines = lines fileContent
        dependencieslist = map unyamlListElem $ getDependenciesBlock fileLines
        newDependenciesList = sort $ packages `union` dependencieslist
        newDependenciesList' = map yamlListElem newDependenciesList
        fileHead = reverse $ dropWhile (not . isDependenciesHdr) $ reverse fileLines
        fileTail = dropWhile isYamlListElem $ tail $ dropWhile (not . isDependenciesHdr) fileLines
    in unlines $ fileHead ++ newDependenciesList' ++ fileTail


getDependenciesBlock :: [String] -> [String]
getDependenciesBlock = takeWhile isYamlListElem . tail . dropWhile (not . isDependenciesHdr)

isYamlListElem = isPrefixOf "- "

isDependenciesHdr :: String -> Bool
isDependenciesHdr = isPrefixOf "dependencies:"

getImportsFromFile :: FilePath -> IO [String]
getImportsFromFile fp = getImports <$> readFile fp
    where
        getImports :: String -> [String]
        getImports = nub . removeBaseModule . map takePackageDeclrs . filter hasPackageDclr . findImportLines . lines

        checkPackageImports :: String -> Bool
        checkPackageImports =  (["{-#","LANGUAGE","PackageImports","#-}"] ==) . words

        findImportLines :: [String] -> [String]
        findImportLines = takeWhile isImport . dropWhile (not . isImport)
            where isImport = isPrefixOf "import"

        takePackageDeclrs :: String -> String
        takePackageDeclrs = filter ('"' /=) . head . tail . words

        hasPackageDclr :: String -> Bool
        hasPackageDclr = any ('"' ==)
        removeBaseModule = filter ("base" /=)

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dirpath = do
    pPrint $ "dirpath"
    pPrint $ dirpath
    directoryFiles <- map (dirpath </>) <$> listDirectory dirpath
    actualFiles <- filterM (fmap not . doesDirectoryExist) directoryFiles
    --absolutePaths <- mapM makeAbsolute actualFiles
    folders <- filterM doesDirectoryExist directoryFiles
    putStrLn $ "directories:"
    pPrint folders
    subfiles <- mapM listAllFiles folders
    return $ reverse $ actualFiles ++ concat subfiles
