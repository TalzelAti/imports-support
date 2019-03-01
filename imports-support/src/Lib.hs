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
import           "optparse-applicative"      Options.Applicative
import           "pretty-simple"       Text.Pretty.Simple
import           "uniplate"         Data.Generics.Uniplate.Data
import           "base"               Control.Arrow
import           "text"                     Data.Text (Text)
import           "filepath"            System.FilePath
import                                Types
import                                Imports.Support.Parser
import                                Imports.Support.Parser.Types

import Control.Exception
--import Debug.Trace
import System.IO.Error


import Debug.Trace
lttrace a b = trace (a ++ ":" ++ show b) b

-- todo: add option to remove redundant files from package.yml
-- todo: imports formatter
-- todo: importify file (add package imports and labels)
-- todo: write tests

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

someFunc :: IO ()
someFunc = do
    fileContent <- readFile "/home/talz/development/imports-support/test.hs"
    let res = either error id $ parseString "/home/talz/development/imports-support/test.hs" fileContent
    pPrint res
    pPrint $ formatImports res
    --execParser opts >>= runCmd
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
           <> help "prints execution plan for path, and generates tempFiles"
            )



runCmd :: SearchOpts -> IO ()
runCmd (SearchOpts dir actionToTake) = do
    cwd <- getCurrentDirectory
    let path = cwd </> dir
    packages <- findPackagesRecursively path
    case actionToTake of
        Execute -> do
            packages' <- prepare packages
            executePackageModification False packages'
        PrintPlan -> do
            reports <- prepareReport packages
            putStrLn "Printing Report"
            forM_ reports $ \(hdr,tree) -> do
                putStrLn ("### " ++ hdr )
                putStrLn ""
                pPrint tree
                putStrLn ""
                pPrint ("############################" :: Text)
                putStrLn ""
                executePackageModification True $ snd $ last reports


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
                    Right stmts -> Right $ HsFileAnnot hsFile hasPackageImports stmts
            return $ case partitionEithers fsAnns of
                ([],fsAnns') -> PackageAnnot (PkgsYaml f:fsAnns')
                (errs,_) -> head errs

    return $ Package packagePath annot

getPkgsModifications :: WorkTree -> IO WorkTree
getPkgsModifications (Package path (PackageAnnot (PkgsYaml pyfp:hsfs))) = do
    let packagesList = nub $ concatMap getPackageNames $
            map snd $ filter fst $
            map (faHasPackageImports &&& faImportsList) hsfs
    return $ Package path $ PkgsFileMods pyfp packagesList
getPkgsModifications dir = return dir

getPackageNames :: [ImportStmt] -> [String]
getPackageNames = nub . removeBaseModule . catMaybes . map _importStmtQualOnly_pkgImport

executePackageModification :: Bool -> WorkTree -> IO ()
executePackageModification copyOnly wt = mapM_ worker $ mods
        where
            mods = universeBi wt
            worker (ErrMsg err) = pPrint err
            worker (PkgsFileMods fp pkgs) = do
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

formatImports :: [ImportStmt] -> [String]
formatImports stmts =
    let details = lttrace "details" $ importCharLengthInfo stmts
    in concatMap (formatImport details) stmts

formatImport :: (Bool,Int,Int,Bool,Int,Bool) -> ImportStmt -> [String]
formatImport ( anyHasQualifiedOnly
             , longestPackageImportName
             , longestName
             , anyHasQualifiedAs
             , longestAlias
             , hasHiding
             )
             (ImportStmt
                 pkgname
                 moduleName
                 extras
                 qual
             ) =
    let (importExtraHdr,importExtra) = maybe ([],[]) resolveExtra extras
    in [unwords $ [ "import" ]
               ++ resolveQualify qual
               ++ [ resolvePkgImport pkgname
                  , (concatModuleName moduleName :: String)
                  ]
               ++ resolveAs qual
               ++ importExtraHdr
       ] ++ importExtra
    where
        padSpaces n = replicate n ' '
        concatModuleName = intercalate "."
        emptyQualified =
            if anyHasQualifiedOnly
                then [padSpaces 9]
                else []
        resolveQualify :: Qualified -> [String]
        resolveQualify QualDef = emptyQualified
        resolveQualify QualOnlyAs{} = ["qualified"]
        resolveQualify QualAs{} = emptyQualified
        resolvePkgImport :: Maybe String -> String
        resolvePkgImport (Just nm) = "\""<> nm <> "\"" <> padSpaces (longestPackageImportName - length nm)
        resolvePkgImport Nothing = replicate (longestPackageImportName + 2) ' '
        resolveAs :: Qualified -> [String]
        resolveAs QualDef = []
        resolveAs (QualOnlyAs nm) = resolveAs' nm
        resolveAs (QualAs nm) = resolveAs' nm
        resolveAs' nm =  [padSpaces $ longestName - moduleNameLength moduleName ,"as", intercalate "." nm] --, padSpaces (longestAlias - length nm)]
        emptyExtra = if hasHiding
                     then [padSpaces 6]
                     else []
        resolveExtra InstancesOnly = resolveExtra' emptyExtra []
        resolveExtra (ImportList imlist) = resolveExtra' emptyExtra imlist
        resolveExtra (HidingList imlist) = resolveExtra' ["hiding"] imlist
        resolveExtra' hdr l
            | length l <= 2 = ( hdr <> [formatImportFuncsOneLiner l]
                              , []
                              )
            | otherwise = (hdr,formatImportFuncs l)
        formatImportFuncs (x:xs) = [tab <> "( " <> x]
                         ++ map (\x' -> tab <> ", " <> x' ) xs
                         ++ [tab <> ")"]
        tab = padSpaces 4
        formatImportFuncsOneLiner [] = "()"
        formatImportFuncsOneLiner (x:xs) = "(" <> x
                         <> concatMap (\x' -> ", " <> x' ) xs
                         <> ")"

moduleNameLength = length . intercalate "."

importCharLengthInfo :: [ImportStmt] -> (Bool,Int,Int,Bool,Int, Bool)
importCharLengthInfo stmts =
    let hasQualifiedOnly = any (isQualifiedOnlyAs. _importStmtQualOnly_qualified) stmts
        hasQualifiedAs =  any (isQualifiedAs . _importStmtQualOnly_qualified) stmts
        longestPackageImportName = maximum $ map length $ catMaybes $ map _importStmtQualOnly_pkgImport stmts
        longestName = maximum $ (++ [0]) $ map (moduleNameLength . _importStmtQualOnly_moduleName) stmts
        longestAlias = maximum $ (++ [0]) $ map moduleNameLength $ catMaybes $ map (getAliasName . _importStmtQualOnly_qualified) stmts
        hasHiding = any isHiding . catMaybes . map _importStmtQualOnly_importExtra $ stmts
    in ( hasQualifiedOnly
       , longestPackageImportName
       , longestName
       , hasQualifiedAs
       , longestAlias
       , hasHiding
       )
    where
        getAliasName :: Qualified -> Maybe ModuleName
        getAliasName QualDef = Nothing
        getAliasName (QualOnlyAs nm) = Just nm
        getAliasName (QualAs nm) = Just nm
        isHiding :: ImportExtra -> Bool
        isHiding HidingList{} = True
        isHiding _ = False
        isQualifiedOnlyAs :: Qualified -> Bool
        isQualifiedOnlyAs QualDef = False
        isQualifiedOnlyAs QualOnlyAs{} = True
        isQualifiedOnlyAs QualAs{} = False

        isQualifiedAs :: Qualified -> Bool
        isQualifiedAs QualDef = False
        isQualifiedAs QualOnlyAs{} = False
        isQualifiedAs QualAs{} = True
