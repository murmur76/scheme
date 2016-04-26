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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- FIXME: Recognize floating point numbers
parseNumber :: Parser LispVal
parseNumber = do sign <- optionMaybe $ char '-'
                 digits <- many1 digit
                 return $ case sign of
                            Nothing -> Number . read $ digits
                            otherwise -> Number . negate . read $ digits

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
parseExpr = try parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

