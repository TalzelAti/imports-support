{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib
    ( someFunc
    ) where

import           "directory"           System.Directory
import           "base"                Data.List
import           "base"                Data.Maybe
import           "base"                Control.Monad
import           "optparse-applicative"      Options.Applicative
import           "pretty-simple"       Text.Pretty.Simple
import           "uniplate"         Data.Generics.Uniplate.Data
import           "base"               Data.Foldable
import           "base"               Control.Arrow
import           "text"                     Data.Text (Text)
import           "filepath"            System.FilePath
import                                Types
--import           "base"                Data.Monoid
--import           "base"               System.IO
--import           "extra"             Control.Monad.Extra


--import Debug.Trace
--lttrace a b = trace (a ++ ":" ++ show b) b

-- todo: remove file print before modification, use conduit instead of io
-- todo: deal with comments
-- todo: deal with multilines
-- todo: deal with qualified imports
-- todo: imports formatter
-- todo: importify file (add package imports and labels)
-- todo: improve errors handling

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



runCmd :: SearchOpts -> IO ()
runCmd (SearchOpts dir actionToTake) = do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    case actionToTake of
        Execute -> do
            packages' <- prepare packages
            pPrint ("------------------------------" :: Text)
            executePackageModification packages'
        PrintPlan -> do
            reports <- prepareReport packages
            putStrLn "Printing Modifications Plan"
            forM_ reports $ \(hdr,tree) -> do
                putStrLn ("### " ++ hdr )
                putStrLn ""
                pPrint tree
                putStrLn ""


prepareReport :: WorkTree -> IO [(String,WorkTree)]
prepareReport packages = foldM worker [("initial",packages)] $ wtTransforms
    where
        worker stgs (hdr,f) = do
            let lastStage = snd $ last stgs
            currentStage <- f lastStage
            return $ stgs ++ [(hdr, currentStage)]

prepare :: WorkTree -> IO WorkTree
prepare packages =
    foldrM ($) packages $ map snd $ reverse wtTransforms

wtTransforms :: [(String,WorkTree -> IO WorkTree)]
wtTransforms = [ ("annotatePackage", transformBiM annotatePackage)
               , ("getPkgsModifications", transformBiM getPkgsModifications)
               ]

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
                then return $ Just $ Package NoAnnotation path
                else do
                    folders <- listFolders' path
                    mPackages <- mapM findPackagesRecursively' folders
                    case catMaybes mPackages of
                        [] -> return Nothing
                        packages -> return $ Just $ Directory NoAnnotation path packages

isHiddenFolder :: [Char] -> Bool
isHiddenFolder path =
    let (_, fileName) = splitFileName path
    in isPrefixOf "." fileName

isHaskellPackage :: FilePath -> IO Bool
isHaskellPackage path = do
    actualFiles <- listFiles path
    return $ any (\f -> isPackageYaml f || isCabalFile f) actualFiles

listFiles :: FilePath -> IO [FilePath]
listFiles path =
    map (path </>) <$> listDirectory path >>=
        filterM (fmap not . doesDirectoryExist)
listFolders' :: FilePath -> IO [[Char]]
listFolders' fp = filter (not . isHiddenFolder) <$> listFolders fp

listFolders :: FilePath -> IO [FilePath]
listFolders path =
    map (path </>) <$> listDirectory path >>=
        filterM doesDirectoryExist

isPackageYaml :: [Char] -> Bool
isPackageYaml = isSuffixOf "package.yaml"
isCabalFile :: [Char] -> Bool
isCabalFile = isSuffixOf ".cabal"



annotatePackage :: WorkTree -> IO WorkTree
annotatePackage (Directory _ nm subdirs) = return $ Directory NoAnnotation nm subdirs
annotatePackage (Package _ packagePath) = do
    files <- listAllFiles packagePath
    let haskellFiles = filter (isSuffixOf ".hs") files
        mPackageYamlFile = listToMaybe $ filter isPackageYaml files
    fsAnns <- forM haskellFiles $ \hsFile -> do
        hsFileContent <- readFile hsFile
        let importsList = findImportLines . lines $  hsFileContent -- optimize the readFile call
            hasPackageImports = any isPackageImports . lines $ hsFileContent
        return $ HsFileAnnot hsFile hasPackageImports importsList
    let annot = case mPackageYamlFile of
            Just f  -> PackageAnnot (PkgsYaml f:fsAnns)
            Nothing -> ErrMsg $ "No stack file in package: " ++ show packagePath
    return $ Package annot packagePath

getPkgsModifications :: WorkTree -> IO WorkTree
getPkgsModifications (Package (PackageAnnot (PkgsYaml pyfp:hsfs)) path) = do
    let packagesList = nub $ concatMap getPackageNames $
            map snd $ filter fst $
            map (faHasPackageImports &&& faImportsList) hsfs
    return $ Package (PkgsFileMods pyfp packagesList) path
getPkgsModifications dir = return dir

executePackageModification :: WorkTree -> IO ()
executePackageModification wt = do
    let mods = universeBi wt
    pPrint mods
    mapM_ worker $ mods
        where
            worker (PkgsFileMods fp pkgs) = do
                packageYamlContent <- readFile fp
                let modifiedContent = modifyPackagesSection pkgs packageYamlContent
                pPrint packageYamlContent
                writeFile fp $ modifiedContent
            worker _ = return ()

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

isYamlListElem :: [Char] -> Bool
isYamlListElem = isPrefixOf "- "

isDependenciesHdr :: String -> Bool
isDependenciesHdr = isPrefixOf "dependencies:"

isPackageImports :: String -> Bool
isPackageImports =  (["{-#","LANGUAGE","PackageImports","#-}"] ==) . words

findImportLines :: [String] -> [String]
findImportLines = takeWhile isImport . dropWhile (not . isImport) . filter (not . null)
    where isImport = isPrefixOf "import"

getPackageNames :: [String] -> [String]
getPackageNames = nub . removeBaseModule . map takePackageDeclrs . filter hasPackageDclr


takePackageDeclrs :: String -> String
takePackageDeclrs = filter ('"' /=) . head . tail . words

hasPackageDclr :: String -> Bool
hasPackageDclr = any ('"' ==)

removeBaseModule :: [String] -> [String]
removeBaseModule = filter ("base" /=)

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dirpath = do
    directoryFiles <- map (dirpath </>) <$> listDirectory dirpath
    actualFiles <- filterM (fmap not . doesDirectoryExist) directoryFiles
    folders <- filterM doesDirectoryExist directoryFiles
    let folders' = filter (not . isHiddenFolder) folders
    subfiles <- mapM listAllFiles folders'
    return $ reverse $ actualFiles ++ concat subfiles
