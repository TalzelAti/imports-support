{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           "directory"           System.Directory
import           "base"                Data.List
import           "base"                Data.Maybe
import           "base"                Data.Monoid
import           "base"                Control.Monad
import           "extra"             Control.Monad.Extra
import           "filepath"            System.FilePath
import           "optparse-applicative"      Options.Applicative
import           "pretty-simple"       Text.Pretty.Simple


import Debug.Trace

lttrace a b = trace (a ++ ":" ++ show b) b

-- add flag to run on full directories and scan haskell packages in them

someFunc :: IO ()
someFunc = execParser opts
    >>= runCmd
    where
        opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Run Import Support"
            )
        options :: Parser SearchOpts
        options = packageOpt

        packageOpt :: Parser SearchOpts
        packageOpt = SearchOpts <$>
           strOption
            ( long "filepath"
           <> short 'd'
           <> showDefault
           <> help "runs imports support on a package provided in path or directory"
            )
           <*> actionOpt
        actionOpt :: Parser Action
        actionOpt = actionFromPrint <$> switch
            ( long "print"
           <> short 'p'
           <> showDefault
           <> help "printsPath"
            )

    --moduleNames <- getImportsFromFile fp
    --print =<< listDirectory "."
    --updatePackageDeps "/home/talz/development/atidot/gapsight/gapsight-server"
    --print $ map checkPackageImports fcs
    --print =<< listAllFiles "/home/talz/development/atidot/gapsight/gapsight-server"

actionFromPrint True = PrintDir
actionFromPrint _    = Modify

data Action = PrintDir
            | Modify

data SearchOpts = SearchOpts FilePath Action



-- ensure argument is a dir

-- todo: ignore pack
runCmd :: SearchOpts -> IO ()
runCmd (SearchOpts dir actionToTake) = do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    let action = case actionToTake of
            Modify -> updatePackageDeps
            PrintDir -> pPrint
    action packages

data WorkTree = Package FilePath
              | Directory FilePath [WorkTree]
              deriving (Show)


findPackagesRecursively :: String -> IO WorkTree
findPackagesRecursively dirpath = do
    res <- findPackagesRecursively' dirpath
    return $ fromMaybe err res
    where
        err = error $ "no packages hiding in directory :" ++ show dirpath
        findPackagesRecursively' :: String -> IO (Maybe WorkTree)
        findPackagesRecursively' path = do
            isPackage <- isHaskellPackage path
            if isPackage
                then return $ Just $ Package path
                else do
                    folders <- listFolders' path
                    mPackages <- mapM findPackagesRecursively' folders
                    case catMaybes mPackages of
                        [] -> return Nothing
                        packages -> return $ Just $ Directory path packages

isHiddenFolder path =
    let (_, fileName) = splitFileName path
    in isPrefixOf "." fileName

isHaskellPackage :: FilePath -> IO Bool
isHaskellPackage path = do
    actualFiles <- listFiles path
    return $ any (\f -> isPackageYaml f || isCabalFile f) actualFiles


listFiles path =
    map (path </>) <$> listDirectory path >>=
        filterM (fmap not . doesDirectoryExist)

listFolders' fp = filter (not . isHiddenFolder) <$> listFolders fp

listFolders path =
    map (path </>) <$> listDirectory path >>=
        filterM doesDirectoryExist




isPackageYaml = isSuffixOf "package.yaml"
isCabalFile = isSuffixOf ".cabal"

updatePackageDeps :: WorkTree -> IO ()
updatePackageDeps (Directory _ pkgs) =
    mapM_ updatePackageDeps pkgs
updatePackageDeps (Package packagePath) = do
    files <- listAllFiles packagePath
    putStrLn $ "files:\n" ++ unlines files
    let haskellFiles = filter (isSuffixOf ".hs") files
        packageYamlFile = head $ filter isPackageYaml files
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
