{-# LANGUAGE PackageImports #-}

module Imports.Support.Formatter
    ( formatImports
    ) where
import           "base"                Data.List
import                                Imports.Support.Parser.Types
import           "base"                Data.Maybe


formatImports :: [ImportStmt] -> [String]
formatImports stmts =
    let details = importCharLengthInfo stmts
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
    let mainClause = unwords $ [ "import" ]
                            ++ resolveQualify qual
                            ++ [ resolvePkgImport pkgname
                               , (concatModuleName moduleName :: String)
                               ]
                           ++ resolveAs qual
        (importExtraHdr,importExtra) = maybe ([],[])
                                             (resolveExtra $ padSpaces mainClauseLength)
                                             extras
        mainClauseLength = length mainClause + 2  + length (concat emptyExtra)
    in [ unwords [mainClause, unwords importExtraHdr]
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
        resolveExtra tab InstancesOnly = resolveExtra' tab emptyExtra []
        resolveExtra tab (ImportList imlist) = resolveExtra' tab emptyExtra imlist
        resolveExtra tab (HidingList imlist) = resolveExtra' tab ["hiding"] imlist
        resolveExtra' tab hdr l
            | length l <= 2 = ( hdr <> [formatImportFuncsOneLiner l]
                              , []
                              )
            | otherwise =
                let (x,xs) = formatImportFuncs l tab
                in (hdr ++ x, xs)
        formatImportFuncs (x:xs) tab = ( ["( " <> x]
                                       , map (\x' -> tab <> ", " <> x' ) xs
                                         ++ [tab <> ")"]
                                       )
        -- formatImportFuncsMultiLines (x:xs) = [tab <> "( " <> x]
        --                                   ++ map (\x' -> tab <> ", " <> x' ) xs
        --                                   ++ [tab <> ")"]
        -- tab = padSpaces 4
        formatImportFuncsOneLiner [] = "()"
        formatImportFuncsOneLiner (x:xs) = "(" <> x
                                        <> concatMap (\x' -> ", " <> x' ) xs
                                        <> ")"

moduleNameLength = length . intercalate "."

importCharLengthInfo :: [ImportStmt] -> (Bool,Int,Int,Bool,Int, Bool)
importCharLengthInfo stmts =
    let hasQualifiedOnly = any (isQualifiedOnlyAs. _importStmtQualOnly_qualified) stmts
        hasQualifiedAs =  any (isQualifiedAs . _importStmtQualOnly_qualified) stmts
        longestPackageImportName = maximum $ (++ [0]) $ map length $ catMaybes $ map _importStmtQualOnly_pkgImport stmts
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
