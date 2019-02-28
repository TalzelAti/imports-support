{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Imports.Support.Parser
    ( parseString
    ) where

import  "parsec" Text.ParserCombinators.Parsec
import  "parsec" Text.Parsec.Combinator


import  "parsec" Text.ParserCombinators.Parsec.Expr
import  "parsec" Text.ParserCombinators.Parsec.Language
import qualified  "parsec" Text.ParserCombinators.Parsec.Token as Token
import                                          Imports.Support.Parser.Types



languageDef =
  emptyDef { Token.commentStart    = "{-"
           , Token.commentEnd      = "-}"
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
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

whileParser :: Parser [ImportStmt]
whileParser = do --sepBy statementP (whiteSpace >> char '\n' >> return ())
    stmts <- sepBy statementP whiteSpace
    return stmts

statementP :: Parser ImportStmt
statementP = choice
    [ try importQualifiedAsP
    , try importQualifiedOnlyAsP
    , importStmtP
    ]

importStmtP :: Parser ImportStmt
importStmtP = do
    reserved "import"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    whiteSpace
    extras <- importExtraParser
    return $ ImportStmt
        mPackageImport
        moduleName
        extras
        QualDef

importQualifiedAsP :: Parser ImportStmt
importQualifiedAsP = do
    reserved "import"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    whiteSpace
    reserved "as"
    whiteSpace
    qualifiedName <- nameParser
    whiteSpace
    extras <- importExtraParser
    return $ ImportStmt
        mPackageImport
        moduleName
        extras
        $ QualAs qualifiedName

importQualifiedOnlyAsP :: Parser ImportStmt
importQualifiedOnlyAsP = do
    reserved "import"
    whiteSpace
    reserved "qualified"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    whiteSpace
    reserved "as"
    whiteSpace
    qualifiedName <- nameParser
    whiteSpace
    extras <- importExtraParser
    return $ ImportStmt
        mPackageImport
        moduleName
        Nothing
        $ QualOnlyAs qualifiedName

mPkgImportPrsr :: Parser (Maybe String)
mPkgImportPrsr = optionMaybe $ do
    char '"'
    name <- many1 (try idenWithDot <|> identifier)
    char '"'
    return $ concat name
    where
        idenWithDot = do
            iden <- identifier
            dot <- char '-'
            return $ iden ++ [dot]

nameParser = sepBy1 identifier (char '.')

importExtraParser :: Parser (Maybe ImportExtra)
importExtraParser = optionMaybe $
      choice [ try hidingListParser
             , try importListParser
             , instancesOnlyParser ]

hidingListParser :: Parser ImportExtra
hidingListParser = do
    reserved "hiding"
    whiteSpace
    char '('
    importList <-
        sepBy1 identifier comma
    char ')'
    whiteSpace
    return $ HidingList importList

importListParser :: Parser ImportExtra
importListParser = do
    char '('
    importList <-
        sepBy1 identifier comma
    char ')'
    whiteSpace
    return $ ImportList importList


instancesOnlyParser :: Parser ImportExtra
instancesOnlyParser = do
    char '('
    whiteSpace
    char ')'
    whiteSpace
    return InstancesOnly

parseString :: FilePath -> String -> Either String [ImportStmt]
parseString fp str =
  case parse whileParser fp str of
    Left e  -> Left $ show e
    Right rs -> Right rs
