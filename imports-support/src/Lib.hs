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
--import           "base"               Data.Foldable
import           "base"               Control.Arrow
import           "text"                     Data.Text (Text)
import           "filepath"            System.FilePath
import                                Types
--import           "base"                Data.Monoid
--impodrt           "base"               System.IO
--import           "extra"             Control.Monad.Extra
import Control.Exception
import Debug.Trace
import System.IO.Error
import qualified "parsec" Text.ParserCombinators.Parsec as P
import qualified "parsec" Text.Parsec.Combinator as P


import qualified "parsec" Text.ParserCombinators.Parsec.Expr as P
import qualified "parsec" Text.ParserCombinators.Parsec.Language as P
import qualified  "parsec" Text.ParserCombinators.Parsec.Token as Token

data Qualified = QualDef | QualOnly  | QualAs ModuleName
    deriving (Show)
type ModuleName = [String]
data ImportStmt =
    ImportStmt
        { _importStmtQualOnly_pkgImport  :: Maybe String
        , _importStmtQualOnly_moduleName :: ModuleName
        , _importStmtQualOnly_importExtra :: Maybe ImportExtra
        , _importStmtQualOnly_qualified :: Qualified
        }

        deriving (Show)

data ImportExtra = HidingList [String] | ImportList [String] | InstancesOnly
    deriving (Show)

languageDef =
  P.emptyDef { Token.commentStart    = "{-"
             , Token.commentEnd      = "-}"
             , Token.commentLine     = "--"
             , Token.identStart      = P.letter
             , Token.identLetter     = P.alphaNum
             , Token.reservedNames   = [ "import"
                                       , "qualified"
                                       , "as"
                                       , "hiding"
                                       ]

             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
comma      = Token.comma     lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep = Token.commaSep    lexer
commaSep1 = Token.commaSep1    lexer
symbol = Token.symbol    lexer

whileParser :: P.Parser [ImportStmt]
whileParser = do --P.sepBy statementP (whiteSpace >> P.char '\n' >> return ())
    many statementP
    where

statementP :: P.Parser ImportStmt
statementP = importStmtP
        -- <|> importQualifiedOnlyP

importStmtP :: P.Parser ImportStmt
importStmtP = do
    reserved "import"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    whiteSpace
    return $ ImportStmt
        mPackageImport
        moduleName
        Nothing
        QualDef

mPkgImportPrsr :: P.Parser (Maybe String)
mPkgImportPrsr = P.optionMaybe $ do
    P.char '"'
    name <- identifier
    P.char '"'
    return  name

nameParser = P.sepBy1 identifier (P.char '.')

        -- mAsPrsr = do
        --     reserved "as"
        --     QualAs <$> identifier
        -- mImportListParser =
        --     importsListParser <|> hidingListParser
        -- importsListParser = do
        --     idens  <- parens $ commaSep identifier
        --     return $ if null idens
        --              then InstancesOnly
        --              else ImportList idens
        -- hidingListParser = do
        --     reserved "hiding"
        --     idens  <- parens $ commaSep1 identifier
        --     return $ if null idens
        --              then InstancesOnly
        --              else ImportList idens

importQualifiedOnlyP :: P.Parser ImportStmt
importQualifiedOnlyP = do undefined
    -- reserved "import"
    -- mQualified <- qualifiedTokenParser
    -- whiteSpace
    -- mPackageImport <- mPkgImportPrsr
    -- moduleName <- nameParser
    -- mAs <- mAsPrsr
    --mImportList <- mImportListParser
    -- let qual = f mPackageImport mAs
    -- return $ lttrace "importStmt" $ ImportStmt
    --     qual
    --     mPackageImport
    --     moduleName
    --     Nothing
    where
        -- qualifiedTokenParser :: P.Parser Qualified
        -- qualifiedTokenParser =
        --     (reserved "qualified" >> whiteSpace >> return (QualOnly []))
        --     <|> (whiteSpace >> return (QualAs []))
        -- mPkgImportPrsr :: P.Parser (Maybe String)
        -- mPkgImportPrsr = P.optionMaybe mPkgImportPrsr'
        -- mPkgImportPrsr' = do
        --     P.char '"'
        --     name <- identifier
        --     P.char '"'
        --     return $ lttrace "just name" name

        -- nameParser = do
        --     whiteSpace
        --     P.sepBy1 identifier (P.char '.')
        -- mAsPrsr = do
        --     reserved "as"
        --     QualAs <$> identifier
        -- mImportListParser =
        --     importsListParser <|> hidingListParser
        -- importsListParser = do
        --     idens  <- parens $ commaSep identifier
        --     return $ if null idens
        --              then InstancesOnly
        --              else ImportList idens
        -- hidingListParser = do
        --     reserved "hiding"
        --     idens  <- parens $ commaSep1 identifier
        --     return $ if null idens
        --              then InstancesOnly
        --              else ImportList idens

parseString :: String -> [ImportStmt]
parseString str =
  case P.parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r



-- bExpression :: Parser BExpr
-- bExpression = buildExpressionParser bOperators bTerm


-- bOperators = [ [Prefix (reserved "not" >> return (Not             ))          ]
--              , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
--                 Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
--              ]



-- bTerm =  parens bExpression
--      <|> (reserved "true"  >> return (BoolConst True ))
--      <|> (reserved "false" >> return (BoolConst False))
--      <|> rExpression

--import Debug.Trace
lttrace a b = trace (a ++ ":" ++ show b) b

-- todo: deal with comments
-- todo: deal with multilines
-- todo: deal with qualified imports
-- todo: add option to remove redundant files from package.yml
-- todo: imports formatter
-- todo: importify file (add package imports and labels)
-- todo: unique temp names

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

someFunc :: IO ()
someFunc = do
    fileContent <- readFile "/home/talz/development/imports-support/test.hs"
    putStrLn "########"
    putStrLn fileContent
    putStrLn "########"
    putStrLn $ show fileContent
    putStrLn "########"
    pPrint $ parseString fileContent
    -- execParser opts >>= runCmd
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
            putStrLn "Printing Modifications Plan"
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
    fsAnns <- forM haskellFiles $ \hsFile -> do
        hsFileContent <- readFile hsFile
        let importsList = findImportLines . lines $  hsFileContent -- optimize the readFile call
            hasPackageImports = any isPackageImports . lines $ hsFileContent
        pPrint hsFile
        pPrint ">>>>>>>>>>>>>>>>>>>>>>>>>"
        pPrint $ importsList
        putStrLn $ head importsList
        pPrint $ parseString (head importsList)
        pPrint "<<<<<<<<<<<<<<<<<<<<<<<<<"
        return $ HsFileAnnot hsFile hasPackageImports importsList
    let annot = case mPackageYamlFile of
            Just f  -> PackageAnnot (PkgsYaml f:fsAnns)
            Nothing -> ErrMsg $ "Error: No stack file in package: " ++ show packagePath
    return $ Package packagePath annot

getPkgsModifications :: WorkTree -> IO WorkTree
getPkgsModifications (Package path (PackageAnnot (PkgsYaml pyfp:hsfs))) = do
    let packagesList = nub $ concatMap getPackageNames $
            map snd $ filter fst $
            map (faHasPackageImports &&& faImportsList) hsfs
    return $ Package path $ PkgsFileMods pyfp packagesList
getPkgsModifications dir = return dir

executePackageModification :: Bool -> WorkTree -> IO ()
executePackageModification copyOnly wt =
    mapM_ worker $ mods
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
                    then
                        pPrint mods
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
