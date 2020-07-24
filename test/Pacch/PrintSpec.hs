{-# LANGUAGE OverloadedStrings #-}

module Pacch.PrintSpec where

import Pacch.Data
import Pacch.Print

import Prelude hiding (Char, Int)

import Test.Hspec

spec :: Spec
spec = do
  describe "decl" $ do
    describe "variable" $ do
      it "pi = 3" $ do
        let output = decl $ VarDecl "pi" $ Val $ Int 3
        output `shouldBe` "pi = 3"

    describe "data" $ do
      it "data Bool = True | False" $ do
        let output = decl $ DataDecl "Bool" (Data [] [("True", []), ("False", [])])
        output `shouldBe` "data Bool = True | False"

      it "data List a = Nil | Cons a (List a)" $ do
        let output = decl $ DataDecl "List" (Data ["a"] [("Nil", []), ("Cons", ["a", "(List a)"])])
        output `shouldBe` "data List a = Nil | Cons a (List a)"

    describe "expr" $ do
      describe "variable" $ do
        it "x" $ do
          let output = expr $ Var ("", "x")
          output `shouldBe` "x"

        it "M.x" $ do
          let output = expr $ Var ("M", "x")
          output `shouldBe` "M.x"

        it "M.M.x" $ do
          let output = expr $ Var ("M.M", "x")
          output `shouldBe` "M.M.x"

      describe "value" $ do
        it "0" $ do
          let output = expr $ Val $ Int 0
          output `shouldBe` "0"

        it "\'a\'" $ do
          let output = expr $ Val $ Char 'a'
          output `shouldBe` "\'a\'"

        it "\"hello\"" $ do
          pending
          -- let output = expr $ Val $ Str "hello"
          -- output `shouldBe` "\"hello\""

        it "\\x -> x" $ do
          let output = expr $ Val $ Abs "x" $ Var ("", "x")
          output `shouldBe` "\\x -> x"

        it "Datum (False)" $ do
          let output = expr $ Val $ Datum 0 []
          output `shouldBe` "D0()"

        it "Datum (Cons 1 Nil)" $ do
          let output = expr $ Val $ Datum 1 [Val $ Int 1, Var ("", "Nil")]
          output `shouldBe` "D1(1, Nil)"

        it "Native (negateInt)" $ do
          let output = expr $ Val $ Native "negateInt" $ pure $ Var ("", "x")
          output `shouldBe` "N(negateInt)"

      describe "application" $ do
        it "(\\x -> x) 1" $ do
          let output = expr $ Val (Abs "x" $ Var ("", "x")) `App` Val (Int 1)
          output `shouldBe` "(\\x -> x) 1"

        it "f 1 2" $ do
          let output = expr $ Var ("", "f") `App` Val (Int 1) `App` Val (Int 2)
          output `shouldBe` "f 1 2"

        it "f (g 2)" $ do
          let output = expr $ Var ("", "f") `App` (Var ("", "g") `App` Val (Int 2))
          output `shouldBe` "f (g 2)"

      describe "pattern match" $ do
        it "case x of { x -> x }" $ do
          let output = expr $ Match (Var ("", "x")) [(PVar ("", "x"), Var ("", "x"))]
          output `shouldBe` "case x of { x -> x }"

        it "case x of { 1 -> 1; x -> x }" $ do
          let output = expr $ Match (Var ("", "x")) [(PVal $ Int 1, Val $ Int 1), (PVar ("", "x"), Var ("", "x"))]
          output `shouldBe` "case x of { 1 -> 1; x -> x }"
