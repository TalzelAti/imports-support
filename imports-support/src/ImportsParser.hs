{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}



module ImportsParser
    ( parseString
    ) where

import qualified "parsec" Text.ParserCombinators.Parsec as P
import qualified "parsec" Text.Parsec.Combinator as P


import qualified "parsec" Text.ParserCombinators.Parsec.Expr as P
import qualified "parsec" Text.ParserCombinators.Parsec.Language as P
import qualified  "parsec" Text.ParserCombinators.Parsec.Token as Token
import                                Types


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
    stmts <- P.sepBy statementP whiteSpace
    P.eof
    return stmts

statementP :: P.Parser ImportStmt
statementP = P.choice
    [ P.try importQualifiedAsP
    , P.try importQualifiedOnlyAsP
    , importStmtP
    ]

importStmtP :: P.Parser ImportStmt
importStmtP = do
    reserved "import"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    return $ ImportStmt
        mPackageImport
        moduleName
        Nothing
        QualDef

importQualifiedAsP :: P.Parser ImportStmt
importQualifiedAsP = do
    reserved "import"
    whiteSpace
    mPackageImport <- mPkgImportPrsr
    whiteSpace
    moduleName <- nameParser
    whiteSpace
    reserved "as"
    whiteSpace
    ImportStmt
        mPackageImport
        moduleName
        Nothing
        . QualAs <$> nameParser

importQualifiedOnlyAsP :: P.Parser ImportStmt
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
    ImportStmt
        mPackageImport
        moduleName
        Nothing
        . QualOnlyAs <$> nameParser

mPkgImportPrsr :: P.Parser (Maybe String)
mPkgImportPrsr = P.optionMaybe $ do
    P.char '"'
    name <- P.many1 (P.try idenWithDot P.<|> identifier)
    P.char '"'
    return $ concat name
    where
        idenWithDot = do
            iden <- identifier
            dot <- P.char '-'
            return $ iden ++ [dot]

nameParser = P.sepBy1 identifier (P.char '.')

parseString :: FilePath -> String -> Either String [ImportStmt]
parseString fp str =
  case P.parse whileParser fp str of
    Left e  -> Left $ show e
    Right rs -> Right rs
