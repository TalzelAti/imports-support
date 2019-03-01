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
import           "base"                Data.Either
import           "base"                Control.Monad
import           "pretty-simple"       Text.Pretty.Simple
import           "uniplate"         Data.Generics.Uniplate.Data
import           "base"               Control.Arrow
import           "text"                     Data.Text (Text)
import           "filepath"            System.FilePath
import                                Types
import                                Imports.Support.CLI
import                                Imports.Support.Parser
import                                Imports.Support.Parser.Types
import                                Imports.Support.Formatter

import Control.Exception
--import Debug.Trace
import System.IO.Error
import           "optparse-applicative"      Options.Applicative


-- import Debug.Trace
-- lttrace a b = trace (a ++ ":" ++ show b) b


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

someFunc :: IO ()
someFunc = do
    -- fileContent <- readFile "/home/talz/development/imports-support/test.hs"
    -- let res = either error id $ parseString "/home/talz/development/imports-support/test.hs" fileContent
    -- pPrint res
    -- pPrint $ formatImports res
    execParser options >>= runCmd
    where



runCmd :: Options -> IO ()
runCmd (PrintOpt dir) =  do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    reports <- prepareReport packages
    putStrLn "Printing Report"
    forM_ reports $ \(hdr,tree) -> do
        putStrLn ("### " ++ hdr )
        putStrLn ""
        pPrint tree
        putStrLn ""
        pPrint ("############################" :: Text)

runCmd (ViewOpt dir) =  do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    packages' <- prepare packages

    executePackageChanges True packages'

runCmd (ExecuteOpt dir) =  do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    packages' <- prepare packages
    executePackageChanges False packages'



prepareReport :: WorkTree -> IO [(String,WorkTree)]
prepareReport packages = foldM worker [("initial",packages)] $ wtTransforms
    where
        worker stgs (hdr,f) = do
            let lastStage = snd $ last stgs
            currentStage <- f lastStage
            return $ stgs ++ [(hdr, currentStage)]

prepare :: WorkTree -> IO WorkTree
prepare packages = snd . last <$> prepareReport packages

wtTransforms :: [(String,WorkTree -> IO WorkTree)]
wtTransforms = [ ("annotatePackage", transformBiM annotatePackage)
               , ("addPkgUpdate", return . transformBi addPkgUpdate)
               , ("formatHsFilesImports", return . transformBi formatHsFilesImports)
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
                then return $ Just $ Package path NoAnnotation
                else do
                    folders <- listFolders' path
                    mPackages <- mapM findPackagesRecursively' folders
                    case catMaybes mPackages of
                        [] -> return Nothing
                        packages -> return $ Just $ Directory path NoAnnotation packages

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
annotatePackage (Directory nm _ subdirs) = return $ Directory nm NoAnnotation subdirs
annotatePackage (Package packagePath _) = do
    files <- listAllFiles packagePath
    let haskellFiles = filter (isSuffixOf ".hs") files
        mPackageYamlFile = listToMaybe $ filter isPackageYaml files
    annot <- case mPackageYamlFile of
        Nothing -> return $ ErrMsg $ "Error: No stack file in package: " ++ show packagePath
        Just f  -> do
            fsAnns <- forM haskellFiles $ \hsFile -> do
                hsFileContent <- readFile hsFile
                let importsList = findImportLines . lines $  hsFileContent -- optimize the readFile call
                    hasPackageImports = any isPackageImports . lines $ hsFileContent
                return $ case parseString hsFile $ unlines importsList of
                    Left err -> Left $ ErrMsg err
                    Right stmts -> Right $ HsFileAnnot hsFile hasPackageImports importsList stmts
            return $ case partitionEithers fsAnns of
                ([],fsAnns') -> PackageAnnot (PkgsYaml f:fsAnns') []
                (errs,_) -> head errs

    return $ Package packagePath annot

addPkgUpdate :: WorkTree -> WorkTree
addPkgUpdate (Package path (PackageAnnot annons@(PkgsYaml pyfp:hsfs) [])) =
    let packagesList = nub $ concatMap getPackageNames $
            map snd $ filter fst $
            map (faHasPackageImports &&& faImportsList) hsfs
    in Package path $ PackageAnnot annons [PkgsFileUpdate pyfp packagesList]
addPkgUpdate dir = dir

getPackageNames :: [ImportStmt] -> [String]
getPackageNames = nub . removeBaseModule . catMaybes . map _importStmtQualOnly_pkgImport

executePackageChanges :: Bool -> WorkTree -> IO ()
executePackageChanges copyOnly wt = mapM_ worker $ mods
        where
            mods = universeBi wt
            worker (ErrMsg err) = pPrint err
            worker (PackageAnnot _ updates) = mapM_ (executeFileUpdate copyOnly) updates
            worker NoAnnotation = return ()

executeFileUpdate :: Bool -> FileUpdate -> IO ()
executeFileUpdate copyOnly (PkgsFileUpdate fp pkgs) = do
    packageYamlContent <- readFile fp
    let modifiedContent = modifyPackagesSection pkgs packageYamlContent
        tempFile = addTempPrefix fp
    removeIfExists tempFile
    writeFile tempFile $ modifiedContent
    if copyOnly
        then do
            return ()
        else do
            copyFile tempFile fp
            removeIfExists tempFile
executeFileUpdate _ _ = return ()

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
findImportLines = takeWhile isImportClause . dropWhile (not . isImport)
    where isImport = isPrefixOf "import "
          isLineComment = isPrefixOf "--" . filter (not . (' '==))
          isMultiLineImport = isPrefixOf " "
          isImportClause x = isImport x || null x || isMultiLineImport x || isLineComment x





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

formatHsFilesImports :: Annotation -> Annotation
formatHsFilesImports (PackageAnnot filesAnnot filesUpdates) =
    let moreFileUpdates = mapMaybe formatHsFile filesAnnot
    in PackageAnnot filesAnnot $ filesUpdates <> moreFileUpdates
    where
        formatHsFile :: FileAnnot -> Maybe FileUpdate
        formatHsFile (HsFileAnnot fp hasPI imports importStmts) =
            let formattedImports = formatImports importStmts
            in Just $ HsFileUpdate fp hasPI imports importStmts formattedImports
        formatHsFile _ = Nothing
formatHsFilesImports wt = wt
