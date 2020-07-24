{-# LANGUAGE OverloadedStrings #-}

module Pacch.Print
  ( decl
  , expr
  , qvar
  ) where

import Pacch.Data (QName, Data (Data), Decl (DataDecl, VarDecl), Expr (App, Match, Val, Var), Pat (PApp, PVal, PVar),
                   Val (Abs, Char, Datum, Int, Native))

import           Prelude (Monoid (mconcat), Ord ((<), (<=)), Semigroup ((<>)), Show (showsPrec), ShowS, String, id,
                          null, otherwise, showParen, showString, shows, ($), (.), (<$>))
import qualified Prelude

import           Data.Bifunctor (Bifunctor (bimap))
import           Data.List      (intersperse)
import           Data.Monoid    (Endo (Endo, appEndo))
import qualified Data.Text      as Text
import           GHC.Show       (appPrec, appPrec1, showSpace)

decl :: Decl -> String
decl a = declPrec 0 a ""

expr :: Expr -> String
expr a = exprPrec 0 a ""

qvar :: QName -> String
qvar a = qvarPrec 0 a ""

declPrec :: Prelude.Int -> Decl -> ShowS
declPrec _ (VarDecl name expr) = showString (Text.unpack name) . showString " = " . exprPrec 0 expr
declPrec _ (DataDecl typConName (Data typArgNames valCons)) =
  showString "data " . appEndo (mconcat $ Endo <$> showString (Text.unpack typConName) : showSpace : ((. showSpace) . showString . Text.unpack <$> typArgNames)) .
  showString "= " . appEndo (mconcat $ Endo <$> intersperse (showString " | ") ((\(name, types) -> showString name . if null types then id else showString " " . appEndo (mconcat $ Endo <$> intersperse showSpace (showString <$> types))) . bimap Text.unpack (Text.unpack <$>) <$> valCons))

exprPrec :: Prelude.Int -> Expr -> ShowS
exprPrec _ (Var name) = qvarPrec 0 name
exprPrec d (Val val) = valPrec d val
exprPrec d (App expr0 expr1) = showParen (appPrec < d) $ exprPrec appPrec expr0 . showSpace . exprPrec appPrec1 expr1
exprPrec _ (Match expr brcs) =
  showString "case " . exprPrec 0 expr . showString " of { " . appEndo (mconcat $ Endo <$> intersperse (showString "; ") (go <$> brcs)) . showString " }"
  where
    go (pat', expr) = patPrec 0 pat' . showString " -> " . exprPrec 0 expr

qvarPrec :: Prelude.Int -> QName -> ShowS
qvarPrec _ (mod, name)
  | Text.null mod = showString (Text.unpack name)
  | otherwise = showString (Text.unpack $ mod <> "." <> name)

valPrec :: Prelude.Int -> Val Expr -> ShowS
valPrec d (Int v) = showsPrec d v
valPrec d (Char v) = showsPrec d v
valPrec d (Abs name expr) = showParen (appPrec <= d) $ showString "\\" . showString (Text.unpack name) . showString " -> " . exprPrec 0 expr
valPrec d (Datum n exprs) = showParen (appPrec < d) $ showString "D" . shows n . showString "(" . appEndo (mconcat $ Endo <$> intersperse (showString ", ") (exprPrec 0 <$> exprs)) . showString ")"
valPrec d (Native ident _) = showParen (appPrec < d) $ showString "N(" . showString ident . showString ")"

patPrec :: Prelude.Int -> Pat -> ShowS
patPrec _ (PVar (mod, name))
  | Text.null mod = showString (Text.unpack name)
  | otherwise = showString (Text.unpack $ mod <> "." <> name)
patPrec d (PVal val)         = pvalPrec d val
patPrec d (PApp pat0 pat1)   = showParen (appPrec < d) $ patPrec 0 pat0 . showSpace . patPrec appPrec1 pat1

pvalPrec :: Prelude.Int -> Val Pat -> ShowS
pvalPrec d (Int v) = showsPrec d v
pvalPrec d (Char v) = showsPrec d v
pvalPrec d (Abs name pat) = showParen (appPrec <= d) $ showString "\\" . showString (Text.unpack name) . showString " -> " . patPrec 0 pat
pvalPrec d (Datum n pats) = showParen (appPrec < d) $ showString "D" . showsPrec appPrec1 n . showString " (" . appEndo (mconcat $ Endo <$> intersperse (showString ", ") (patPrec 0 <$> pats)) . showString ")"
pvalPrec d (Native ident _) = showParen (appPrec < d) $ showString "N(" . showString ident . showString ")"
