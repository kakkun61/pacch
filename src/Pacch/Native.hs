{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Pacch.Native
  ( natives
  , negateInt
  , plusInt
  ) where

import           Pacch.Data (Context (Context), Expr (Val, Var), Name, Namespace, Val (Abs, Int, Native), newName)
import qualified Pacch.Eval as Eval

import Prelude (Applicative (pure), Maybe (Nothing), MonadFail (fail), Monoid (mempty), Num (negate, (+)), ($), (<$>))

import           Control.Monad.ReaderState.Strict (ReaderState)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map

type Native = ReaderState Context Expr

natives :: Namespace
natives =
  go
    [ ( "Pacch.Int"
      , [ ("negateInt", negateInt)
        , ("plusInt", plusInt)
        ]
      )
    ]
  where
    go :: Map Name (Map Name Native) -> Namespace
    go = ((\nvs -> Context mempty (Map.mapKeys ("",) nvs) mempty 0 Nothing) <$>)

negateInt :: Native
negateInt = do
  name <- newName
  pure $
    Val $ Abs name $ Val $ Native "Pacch.Int.negateInt" $  do
      expr <- Eval.expr $ Var ("", name)
      case expr of
        Val (Int n) -> pure $ Val $ Int $ negate n
        _           -> fail "expected: Int"

plusInt :: Native
plusInt = do
  name0 <- newName
  name1 <- newName
  pure $
     Val $ Abs name0 $ Val $ Abs name1 $ Val $ Native "Pacch.Int.plusInt" $ do
      expr0 <- Eval.expr $ Var ("", name0)
      expr1 <- Eval.expr $ Var ("", name1)
      case (expr0, expr1) of
        (Val (Int n0), Val (Int n1)) -> pure $ Val $ Int $ n0 + n1
        _                            -> fail "expected: Int"
