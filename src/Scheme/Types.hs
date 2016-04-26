{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.Types
  ( Env
  , LispError(..)
  , LispVal(..)
  , IOThrowsError
  , showVal
  , ThrowsError
  , EnvIOThrowsError(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import System.IO
import Text.Parsec

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

-- FIXME:: Use Data.StRef instead of Data.IORef.
type Env = IORef [(String, IORef LispVal)]
newtype EnvIOThrowsError a = EnvIOThrowsError {
        run :: (ReaderT Env (ExceptT LispError IO) a)
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError LispError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

