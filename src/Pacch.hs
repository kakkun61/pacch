{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Pacch
  ( module Pacch.Data
  , module Pacch.Native
    -- * Parsers
  , Parse.Parser
  , Parse.parse
  , modParser
  , imptParser
  , declParser
  , exprParser
  , qvarParser
    -- * Evaluators
  , evalMod
  , evalDecl
  , evalExpr
    -- * Printers
  , printDecl
  , printExpr
  , printQVar
  ) where

import Pacch.Data (Context (Context, internalName, nativeVars, pats, vars), Decl (DataDecl, VarDecl),
                   Expr (App, Match, Val, Var), Import (Import), Name, Namespace, Pat (PApp, PVal, PVar),
                   Qualified (Qualified), Type, Val (Abs, Char, Datum, Int, Native), import', Module (Module), lookupVar, QName)

import qualified Pacch.Eval   as Eval
import           Pacch.Native (natives)
import qualified Pacch.Parse  as Parse
import qualified Pacch.Print  as Print

import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (MonadState)

-- Parsers

modParser :: Parse.Parser Module
modParser = Parse.mod

declParser :: Parse.Parser Decl
declParser = Parse.decl

exprParser :: Parse.Parser Expr
exprParser = Parse.expr

imptParser :: Parse.Parser Import
imptParser = Parse.impt

qvarParser :: Parse.Parser QName
qvarParser = Parse.qvar

-- Evaluators

evalMod :: (MonadFail m, MonadState Namespace m, MonadReader Namespace m) => Module -> m ()
evalMod = Eval.mod

evalDecl :: (MonadFail m, MonadState Context m) => Decl -> m ()
evalDecl = Eval.decl

evalExpr :: (MonadFail m, MonadState Context m, MonadReader Context m) => Expr -> m Expr
evalExpr = Eval.expr

-- Printers

printDecl :: Decl -> String
printDecl = Print.decl

printExpr :: Expr -> String
printExpr = Print.expr

printQVar :: QName -> String
printQVar = Print.qvar
