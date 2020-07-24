{-# LANGUAGE OverloadedStrings #-}

module Pacch.EvalSpec where

import           Pacch        ()
import           Pacch.Data
import qualified Pacch.Eval   as Eval
import           Pacch.Native

import Prelude hiding (Char, Int)

import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.ReaderState.Strict (ReaderStateT, runReaderStateT)
import           Control.Monad.State.Strict       (evalStateT, execStateT, runStateT)
import           Data.Default.Class               (Default (def))
import qualified Data.Map.Strict                  as Map
import           Test.Hspec

spec :: Spec
spec = do
  describe "expr" $ do
    describe "value" $ do
      it "1 --> 1" $ do
        let expr = Val $ Int 1
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

      it "\'a\' --> \'a\'" $ do
        let expr = Val $ Char 'a'
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Char 'a')

    describe "application" $ do
      it "(\\x -> x) 1 --> 1" $ do
        let expr = App (Val $ Abs "x" $ Var ("", "x")) (Val $ Int 1)
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

      it "(\\x -> \\y -> x) 1 2 --> 1" $ do
        let expr = Val (Abs "x" $ Val $ Abs "y" $ Var ("", "x")) `App` Val (Int 1) `App` Val (Int 2)
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

      it "(\\x -> \\y -> y) 1 2 --> 2" $ do
        let expr = Val (Abs "x" $ Val $ Abs "y" $ Var ("", "y")) `App` Val (Int 1) `App` Val (Int 2)
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 2)

      it "(\\x -> \\y -> x y) (\\x -> x) 1 --> 2" $ do
        let expr = Val (Abs "x" $ Val $ Abs "y" $ Var ("", "x") `App` Var ("", "y")) `App` Val (Abs "x" $ Var ("", "x")) `App` Val (Int 1)
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

    describe "native application" $ do
      it "Pacch.Int.negateInt 1 --> (-1)" $ do
        let expr = App (Var ("Pacch.Int", "negateInt")) (Val $ Int 1)
        ctx <- execStateT (runReaderT (import' $ Import "Pacch.Int" (Just Qualified) Nothing ["negateInt"]) natives) def
        result <- evalT (Eval.expr expr) ctx
        result `shouldBe` Val (Int (-1))

      it "Pacch.Int.plusInt 1 2 --> 3" $ do
        let expr = App (App (Var ("Pacch.Int", "plusInt")) (Val $ Int 1)) (Val $ Int 2)
        ctx <- execStateT (runReaderT (import' $ Import "Pacch.Int" (Just Qualified) Nothing ["plusInt"]) natives) def
        result <- evalT (Eval.expr expr) ctx
        result `shouldBe` Val (Int 3)

    describe "pattern match" $ do
      it "case 1 of { 1 -> 1 } --> 1" $ do
        let expr = Match (Val $ Int 1) [(PVal $ Int 1, Val $ Int 1)]
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

      it "case 1 of { x -> x } --> 1" $ do
        let expr = Match (Val $ Int 1) [(PVar ("", "x"), Var ("", "x"))]
        result <- evalT (Eval.expr expr) def
        result `shouldBe` Val (Int 1)

      it "case True of { True -> 1 } --> 1" $ do
        ctx <- execT (Eval.decl $ DataDecl "Bool" (Data [] [("True", []), ("False", [])])) def
        let expr = Match (Var ("", "True")) [(PVar ("", "True"), Val $ Int 1)]
        result <- evalT (Eval.expr expr) ctx
        result `shouldBe` Val (Int 1)

      it "case True of { True -> 1 } --> 1" $ do
        ctx <- execT (Eval.decl $ DataDecl "Bool" (Data [] [("True", []), ("False", [])])) def
        let expr = Match (Var ("", "True")) [(PVar ("", "True"), Val $ Int 1)]
        result <- evalT (Eval.expr expr) ctx
        result `shouldBe` Val (Int 1)

      it "case Cons 1 Nil of { Cons a Nil -> a } --> 1" $ do
        ctx <- execT (Eval.decl $ DataDecl "List" (Data ["a"] [("Nil", []), ("Cons", ["a", "List a"])])) def
        let expr = Match (Var ("", "Cons") `App` Val (Int 1) `App` Var ("", "Nil")) [(PVar ("", "Cons") `PApp` PVar ("", "a") `PApp` PVar ("", "Nil"), Var ("", "a"))]
        result <- evalT (Eval.expr expr) ctx
        result `shouldBe` Val (Int 1)

  describe "decl" $ do
    describe "variable decl" $ do
      it "pi = 3" $ do
        ctx <- execT (Eval.decl $ VarDecl "pi" $ Val $ Int 3) def
        ctx `shouldSatisfy` (Map.member ("", "pi") . vars)

    describe "data decl" $ do
      it "data Bool = True | False" $ do
        ctx <- execT (Eval.decl $ DataDecl "Bool" (Data [] [("True", []), ("False", [])])) def
        ctx `shouldSatisfy` (Map.member ("", "True") . vars)
        ctx `shouldSatisfy` (Map.member ("", "False") . vars)

      it "data List a = Nil | Cons a (List a)" $ do
        ctx <- execT (Eval.decl $ DataDecl "List" (Data ["a"] [("Nil", []), ("Cons", ["a", "List a"])])) def
        ctx `shouldSatisfy` (Map.member ("", "Nil") . vars)
        ctx `shouldSatisfy` (Map.member ("", "Cons") . vars)

  describe "mod" $ do
    it "Module M where" $ do
      ns <- execT (Eval.mod $ Module "M" [] []) mempty
      ns `shouldSatisfy` Map.member "M"
      Map.lookup "M" ns `shouldSatisfy` maybe False (Map.null . vars)

    it "module M where\\npi = 3" $ do
      ns <- execT (Eval.mod $ Module "M" [] [VarDecl "pi" $ Val $ Int 3]) mempty
      ns `shouldSatisfy` Map.member "M"
      Map.lookup "M" ns `shouldSatisfy` maybe False (Map.member ("", "pi") . vars)

    it "import M" $ do
      ns <-
        flip execT mempty$ do
          Eval.mod $ Module "M" [] [VarDecl "pi" $ Val $ Int 3]
          Eval.mod $ Module "Main" [Import "M" Nothing Nothing []] []
      ns `shouldSatisfy` Map.member "Main"
      Map.lookup "Main" ns `shouldSatisfy` maybe False (Map.member ("", "pi") . vars)

evalT :: Monad m => ReaderStateT s m a -> s -> m a
evalT = evalStateT . runReaderStateT

runT :: Monad m => ReaderStateT s m a -> s -> m (a, s)
runT = runStateT . runReaderStateT

execT :: Monad m => ReaderStateT s m a -> s -> m s
execT = execStateT . runReaderStateT
