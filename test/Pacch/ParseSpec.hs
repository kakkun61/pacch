{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Pacch.ParseSpec where

import Pacch.Data
import Pacch.Parse

import Prelude hiding (Char, Int, Integral (mod))

import Test.Hspec

import           Control.Exception (IOException)
import qualified Text.Megaparsec   as M

spec :: Spec
spec = do
  describe "expr" $ do
    describe "variable" $ do
      it "x" $ do
        input <- parse (expr <* M.eof) "" "x"
        input `shouldBe` Var ("", "x")

      it "_" $ do
        input <- parse (expr <* M.eof) "" "_"
        input `shouldBe` Var ("", "_")

      it "_x" $ do
        input <- parse (expr <* M.eof) "" "_x"
        input `shouldBe` Var ("", "_x")

      it "M.x" $ do
        input <- parse (expr <* M.eof) "" "M.x"
        input `shouldBe` Var ("M", "x")

      it "M.M.x" $ do
        input <- parse (expr <* M.eof) "" "M.M.x"
        input `shouldBe` Var ("M.M", "x")

      describe "keyword cannot be used as variable" $ do
        it "case" $ do
          shouldThrow @IOException @_
            (parse (expr <* M.eof) "" "case")
            (\_ -> True)

    describe "value" $ do
      it "0" $ do
        input <- parse (expr <* M.eof) "" "0"
        input `shouldBe` Val (Int 0)

      it "\'a\'" $ do
        input <- parse (expr <* M.eof) "" "\'a\'"
        input `shouldBe` Val (Char 'a')

      it "\"hi\"" $ do
        input <- parse (expr <* M.eof) "" "\"hi\""
        input `shouldBe` Val (Datum 1 [Val $ Char 'h', Val $ Datum 1 [Val $ Char 'i', Val $ Datum 0 []]])

      it "\\x -> x" $ do
        input <- parse (expr <* M.eof) "" "\\x -> x"
        input `shouldBe` Val (Abs "x" $ Var ("", "x"))

    describe "application" $ do
      it "(\\x -> x) 1" $ do
        input <- parse (expr <* M.eof) "" "(\\x -> x) 1"
        input `shouldBe` Val (Abs "x" $ Var ("", "x")) `App` Val (Int 1)

      it "f 1 2" $ do
        input <- parse (expr <* M.eof) "" "f 1 2"
        input `shouldBe` (Var ("", "f") `App` Val (Int 1) `App` Val (Int 2))

      it "f (g 2)" $ do
        input <- parse (expr <* M.eof) "" "f (g 2)"
        input `shouldBe` (Var ("", "f") `App` (Var ("", "g") `App` Val (Int 2)))

    describe "pattern match" $ do
      it "case x of { x -> x }" $ do
        input <- parse (expr <* M.eof) "" "case x of { x -> x }"
        input `shouldBe` Match (Var ("", "x")) [(PVar ("", "x"), Var ("", "x"))]

      it "case x of { 1 -> 1; x -> x }" $ do
        input <- parse (expr <* M.eof) "" "case x of { 1 -> 1; x -> x }"
        input `shouldBe` Match (Var ("", "x")) [(PVal $ Int 1, Val $ Int 1), (PVar ("", "x"), Var ("", "x"))]

    describe "comment" $ do
      it "line comment" $ do
        input <- parse (decl <* M.eof) "" "one = 1 -- comment"
        input `shouldBe` VarDecl "one" (Val $ Int 1)

      it "block comment" $ do
        input <- parse (decl <* M.eof) "" "one {- \n -} = 1"
        input `shouldBe` VarDecl "one" (Val $ Int 1)

  describe "decl" $ do
    describe "variable" $ do
      it "pi = 3" $ do
        input <- parse (decl <* M.eof) "" "pi = 3"
        input `shouldBe` VarDecl "pi" (Val $ Int 3)

      it "const a _ = a" $ do
        input <- parse (decl <* M.eof) "" "const a _ = a"
        input `shouldBe` VarDecl "const" (Val $ Abs "a" $ Val $ Abs "_" $ Var ("", "a"))

    describe "data" $ do
      it "data Bool = True | False" $ do
        input <- parse (decl <* M.eof) "" "data Bool = True | False"
        input `shouldBe` DataDecl "Bool" (Data [] [("True", []), ("False", [])])

      it "data List a = Nil | Cons a (List a)" $ do
        input <- parse (decl <* M.eof) "" "data List a = Nil | Cons a (List a)"
        input `shouldBe` DataDecl "List" (Data ["a"] [("Nil", []), ("Cons", ["a", "(List a)"])])

  describe "mod" $ do
    it "one = 1\\ntwo = 2" $ do
      input <- parse (mod <* M.eof) "" "one = 1\ntwo = 2"
      input `shouldBe` Module "Main" [] [VarDecl "one" $ Val $ Int 1, VarDecl "two" $ Val $ Int 2]

    it "module M where\\none = 1" $ do
      input <- parse (mod <* M.eof) "" "module M where\none = 1"
      input `shouldBe` Module "M" [] [VarDecl "one" $ Val $ Int 1]

    it "import M" $ do
      input <- parse (mod <* M.eof) "" "import M"
      input `shouldBe` Module "Main" [Import "M" Nothing Nothing []] []

    it "import qualified M" $ do
      input <- parse (mod <* M.eof) "" "import qualified M"
      input `shouldBe` Module "Main" [Import "M" (Just Qualified) Nothing []] []

    it "import M as N" $ do
      input <- parse (mod <* M.eof) "" "import M as N"
      input `shouldBe` Module "Main" [Import "M" Nothing (Just "N") []] []

    it "import qualified M as N" $ do
      input <- parse (mod <* M.eof) "" "import qualified M as N"
      input `shouldBe` Module "Main" [Import "M" (Just Qualified) (Just "N") []] []

    describe "comment" $ do
      it "line comment (before)" $ do
        input <- parse (mod <* M.eof) "" "-- comment\none = 1"
        input `shouldBe` Module "Main" [] [VarDecl "one" (Val $ Int 1)]

      it "line comment (after)" $ do
        input <- parse (mod <* M.eof) "" "one = 1\n-- comment"
        input `shouldBe` Module "Main" [] [VarDecl "one" (Val $ Int 1)]

      it "block comment" $ do
        input <- parse (mod <* M.eof) "" "{- comment -}\none = 1"
        input `shouldBe` Module "Main" [] [VarDecl "one" (Val $ Int 1)]
