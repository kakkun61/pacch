{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pacch.Eval
  ( expr
  , expr'
  , pat
  , decl
  , mod
  ) where

import Pacch.Data (Context, Data (Data), Decl (DataDecl, DataDecl, VarDecl), Expr (App, Match, Val, Var),
                   Module (Module), Namespace, Pat (PApp, PVal, PVar), Val (Abs, Char, Datum, Int, Native), import',
                   insertModule, insertPat, insertVar, lookupPat, lookupVar, memberModule, newName)

import Prelude (Applicative (pure), Either (Left, Right), Eq ((==), (/=)), Maybe (Just, Nothing), Monad ((>>), (>>=)),
                MonadFail (fail), Monoid (mconcat, mempty), Traversable (traverse), and, flip, foldr, fst, length,
                maybe, otherwise, sequenceA, snd, uncurry, unzip, zip, ($), (.), (<$>), (||))

import           Control.Monad                    (replicateM, unless)
import           Control.Monad.Reader             (MonadReader (reader), ask, runReaderT)
import           Control.Monad.ReaderState.Strict (ReaderStateT, runReaderState, runReaderStateT)
import           Control.Monad.State.Strict       (MonadState, evalState, execStateT, get, put)
import           Control.Monad.Trans              (MonadTrans (lift))
import           Data.Bifunctor                   (bimap)
import           Data.Char                        (isDigit, isUpper)
import           Data.Default.Class               (Default (def))
import           Data.Foldable                    (for_)
import           Data.Functor                     (($>))
import           Data.Maybe                       (isJust)
import           Data.Monoid                      (Ap (Ap, getAp), First (First, getFirst))
import qualified Data.Text                        as Text

-- lazy evaluation, call by need, large step
expr :: (MonadFail m, MonadState Context m, MonadReader Context m) => Expr -> m Expr
expr e = do
  e' <- expr' e
  if e == e'
    then pure e
    else do
      e'' <- expr e'
      case e of
        Var name | e' /= e'' -> insertVar name e''
        _ -> pure ()
      pure e''

-- lazy evaluation, call by need, small step
expr' :: (MonadFail m, MonadState Context m, MonadReader Context m) => Expr -> m Expr
expr' (Var name) = do
  e <- lookupVar name
  case e of
    Left e  -> pure e
    Right e -> reader $ evalState $ runReaderState e
expr' val@Val {} = pure val
expr' (App (Val (Abs n (Val (Native _ ntv)))) e1) = do
  insertVar ("", n) e1
  ntv
expr' (App (Val (Abs n body)) e1) = do
  insertVar ("", n) e1
  pure body
expr' (App e0 e1) = do
  e0 <- expr' e0
  pure $ App e0 e1
expr' (Match tgt brcs) = do
  tgt <- expr tgt
  rs <- traverse (match tgt) $ fst <$> brcs
  let
    r = getFirst $ mconcat $ First . (\(m, b) -> (, b) <$> m) <$> zip rs (snd <$> brcs)
  case r of
    Just (mu, body) -> mu $> body
    Nothing         -> fail "no matched patterns"
  where
    -- m: maybe fail
    -- Maybe: match or not
    -- n: result action
    match :: (MonadFail m, MonadState Context m, MonadReader Context m, MonadState Context n) => Expr -> Pat -> m (Maybe (n ()))
    match tgt p = do
      p <- pat p -- TODO
      case (p, tgt) of
        (PVar n, _) -> pure $ Just $ insertVar n tgt
        (PVal (Int p), Val (Int v)) | p == v -> pure $ Just $ pure ()
                                    | otherwise -> pure Nothing
        (PVal Int {}, Val {}) -> pure Nothing
        (PVal (Char p), Val (Char v)) | p == v -> pure $ Just $ pure ()
                                    | otherwise -> pure Nothing
        (PVal Char {}, Val {}) -> pure Nothing
        (PVal (Datum n pats), Val (Datum m es))
          | n == m ->
              if length pats == length es
                then do
                  (b, a) <- bimap and mconcat . unzip . ((\ma -> (isJust ma, maybe mempty Ap ma)) <$>) <$> sequenceA ((\(t, p) -> expr t >>= flip match p) <$> zip es pats)
                  if b
                    then pure $ Just $ getAp a
                    else pure Nothing
                else fail "mismatch of the number of arguments"
          | otherwise -> pure Nothing
        (PVal Datum {}, Val {}) -> pure Nothing
        _ -> pure Nothing

-- Strict evaluation, call by value
pat :: (MonadFail m, MonadState Context m, MonadReader Context m) => Pat -> m Pat
pat p@(PVar name@(_, n))
  | Text.null n =fail "empty pattern variable name"
  | c <- Text.head n
  , isUpper c || isDigit c = do
      p <- lookupPat name
      pat p
  | otherwise = pure p
pat (PVal (Datum n pats)) = do
  pats <- traverse pat pats
  pure $ PVal $ Datum n pats
pat p@(PVal _) = pure p
pat (PApp p0 p1) = do
  p1 <- pat p1
  p0 <- pat p0
  case p0 of
    (PVal (Abs n p0)) -> do
      insertPat ("", n) p1
      pat p0
    _ -> fail "other than Abs even after PApp"

decl :: (MonadFail m, MonadState Context m) => Decl -> m ()
decl (VarDecl name e) = insertVar ("", name) e
decl (DataDecl _typConName (Data _typArgNames valCons)) = do
  for_ (zip valCons [0 ..]) $ \((valConName, types), n) -> do
    names <- replicateM (length types) newName
    let
      makeVar valCon varCon = uncurry ($) $ foldr (aValArg valCon varCon) (valCon . Datum n, []) names
      aValArg valCon varCon valArgName (abs, t) = (valCon . Abs valArgName . abs, varCon valArgName:t)
    insertPat ("", valConName) $ makeVar PVal (PVar . ("",))
    insertVar ("", valConName) $ makeVar Val (Var . ("",))

mod :: forall m. (MonadFail m, MonadState Namespace m, MonadReader Namespace m) => Module -> m ()
mod (Module mod impts decls) = do
  already <- memberModule mod
  unless already $ do
    ns <- ask
    let
      act :: ReaderStateT Context m ()
      act = do
        ctx <- get
        ctx <- lift $ execStateT (runReaderT (foldr ((>>) . import') (pure ()) impts) ns) ctx
        put ctx
        foldr ((>>) . decl) (pure ()) decls
    ctx <- execStateT (runReaderStateT act) def
    insertModule mod ctx
