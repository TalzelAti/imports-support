#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [optparse-applicative])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module ImportsMaintainer where

import           "directory"           System.Directory
import           "base"                Data.List
import           "base"                Data.Monoid
import           "base"                Control.Monad
import           "filepath"            System.FilePath
import           "optparse-applicative"      Options.Applicative

import Debug.Trace

lttrace a b = trace (a ++ ":" ++ show b) b

main :: IO ()
main =  do
    fp <- execParser opts
    updatePackageDeps fp
    where
        opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Run the common server"
            )

        options :: Parser FilePath
        options = strOption
            ( long "filepath"
           <> short 'f'
           <> showDefault
           <> help "Path to Haskell Package with package.yaml inside;"
            )

    --moduleNames <- getImportsFromFile fp
    --print $ map checkPackageImports fcs 
    --print =<< listDirectory "."
    --updatePackageDeps "/home/talz/development/atidot/gapsight/gapsight-server" 
    --print =<< listAllFiles "/home/talz/development/atidot/gapsight/gapsight-server"






-- ensure argument is a dir

-- todo: ignore pack

updatePackageDeps packagePath = do
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
    directoryFiles <- map (dirpath </>) <$> listDirectory dirpath 
    actualFiles <- filterM (fmap not . doesDirectoryExist) directoryFiles
    --absolutePaths <- mapM makeAbsolute actualFiles 
    folders <- filterM doesDirectoryExist directoryFiles
    subfiles <- mapM (listAllFiles . (dirpath </>)) folders
    return $ reverse $ actualFiles ++ concat subfiles
    