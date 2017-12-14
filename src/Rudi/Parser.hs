module Rudi.Parser
    (
        rudiStatement,
        parseFile,
        parseExpr,
        parseStatement
    ) where

import Text.ParserCombinators.Parsec

import Rudi.Types

-- Rudi File Parser
rudiFileParser :: CharParser st RudiFile
rudiFileParser = RudiFile <$> sepEndBy (try rudiStatement <|> try rudiComment <|> pure EmptyStatement) (char '\n')

rudiComment :: CharParser st Statement
rudiComment = do
    spaces
    string "--"
    spaces

    many $ noneOf "\n"

    return EmptyStatement

rudiStatement :: CharParser st Statement
rudiStatement = importParser <|>
                definitionParser

importParser :: CharParser st Statement
importParser = do
    string "import"
    spaces
    moduleName <- many1 (letter <|> char '.')

    return $ Import moduleName

definitionParser :: CharParser st Statement
definitionParser = do
    n <- applyList

    spaces
    choice [string "->", string "→"]
    spaces

    rep <- applyList

    return $ Define n rep

-- Parses: S, K, "VarName"
identifierParser :: CharParser st Expr
identifierParser = do
    name <- many1 (noneOf " \n\r()-→>")

    return $ case name of
                "K" -> K
                "S" -> S
                _ -> Var name

-- Parses: "(Expr)"
parenParser :: CharParser st Expr
parenParser = between (char '(') (char ')') (applyList <|> identifierParser <|> parenParser)

-- Parses: "Expr Expr Expr"
applyList :: CharParser st Expr
applyList = do
    exprs <- sepEndBy1 (identifierParser <|> parenParser) $ char ' '

    return $ foldl1 Apply exprs

parseExpr :: String -> Expr
parseExpr str = case parse applyList "Error: " str of
                    Left err -> error $ show err
                    Right expr -> expr

parseFile :: String -> RudiFile
parseFile str = case parse rudiFileParser "Error: " str of
                    Left err -> error $ show err
                    Right rudiFile -> rudiFile

parseStatement :: String -> Statement
parseStatement str = case parse rudiStatement "Error :" str of
                        Left err -> error $ show err
                        Right statement -> statement


