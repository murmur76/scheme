module Scheme.Parser
  ( readExpr
  , readExprList
  ) where

import Control.Monad
import Control.Monad.Except

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Scheme.Types

-- FIXME: Ignore line starting with ;

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

between' :: Char -> Char -> Parser a -> Parser a
between' open close = between (char open) (char close)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseString :: Parser LispVal
parseString = between' '"' '"' $ liftM String $ many (noneOf "\"")

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- FIXME: Recognize floating point numbers
-- FIXME: Recognize negative numbers
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> (between' '(' ')' $ (try parseList) <|> parseDottedList)

