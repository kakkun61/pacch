{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Pacch.Data
  ( Module (..)
  , Decl (..)
  , Expr (..)
  , Data (..)
  , Pat (..)
  , Val (..)
  , Context (..)
  , Namespace
  , Name
  , QName
  , Type
  , Import (..)
  , Qualified (..)
  , newName
  , insertVar
  , lookupVar
  , insertPat
  , lookupPat
  , insertModule
  , lookupModule
  , memberModule
  , import'
  ) where

import           Prelude (Applicative (pure), Bool (False), Either (Left, Right), Eq ((==)), Maybe (Just, Nothing),
                          MonadFail (fail), Num ((+)), Ord ((>)), Semigroup ((<>)), Show (show, showsPrec), String,
                          Traversable (sequence), Word, otherwise, showParen, showString, snd, ($), (++), (.), (<$>))
import qualified Prelude

import           Control.Monad.Reader             (MonadReader (ask), asks, runReaderT)
import           Control.Monad.ReaderState.Strict (ReaderState)
import           Control.Monad.State.Strict       (MonadState (state))
import           Data.Bifunctor                   (Bifunctor (second), first)
import           Data.Default.Class               (Default (def))
import           Data.Foldable                    (for_)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid                      (Monoid (mempty))
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

type Type = Text

data Module =
  Module
    { name  :: Name
    , impts :: [Import]
    , decls :: [Decl]
    }
  deriving (Show, Eq)

data Decl
  = VarDecl Name Expr
  | DataDecl Name Data
  deriving (Show, Eq)

type Name = Text

-- | Qualified name.
type QName = (Name, Name)

data Expr
  = Var QName
  | Val (Val Expr)
  | App Expr Expr
  | Match Expr [(Pat, Expr)]
  deriving (Show, Eq)

infixl 9 `App`

data Val t
  = Int Prelude.Int
  | Char Prelude.Char
  | Abs Name t
  | Datum Word [t]
  | Native String (forall m. (MonadFail m, MonadState Context m, MonadReader Context m) => m t)

instance Show t => Show (Val t) where
  showsPrec d (Int v) = showParen (d > 10) $ showString "Int " . showsPrec 11 v
  showsPrec d (Char v) = showParen (d > 10) $ showString "Char " . showsPrec 11 v
  showsPrec d (Abs name expr) = showParen (d > 10) $ showString "Abs " . showsPrec 11 name . showString " " . showsPrec 11 expr
  showsPrec d (Datum n dat) = showParen (d > 10) $ showString "Datum " . showsPrec 11 n . showString " " . showsPrec 11 dat
  showsPrec d (Native ident _) = showParen (d > 10) $ showString "Native " . showsPrec 11 ident . showString " _"

instance Eq t => Eq (Val t) where
  Int v0 == Int v1 = v0 == v1
  Char v0 == Char v1 = v0 == v1
  Abs n0 e0 == Abs n1 e1 = (n0, e0) == (n1, e1)
  Datum n0 d0 == Datum n1 d1 = (n0, d0) == (n1, d1)
  Native n0 _ == Native n1 _ = n0 == n1
  _ == _ = False

data Data =
  Data
    [Name] -- ^ type parameters
    [(Name, [Type])] -- ^ value constructors
  deriving (Show, Eq)

data Pat
  = PVar QName
  | PVal (Val Pat)
  | PApp Pat Pat
  deriving (Show, Eq)

data Context =
  Context
    { vars         :: Map QName Expr
    , nativeVars   :: Map QName (ReaderState Context Expr)
    , pats         :: Map QName Pat
    , internalName :: Word
    , enclosing    :: Maybe Context
    }

instance Show Context where
  showsPrec d Context { vars, nativeVars, pats, internalName } = showString "Context { vars = " . showsPrec d vars . showString ", nativeVars = " . showsPrec d (Map.keys nativeVars) . showString ", pats = " . showsPrec d pats . showString ", internalName = " . showsPrec d internalName . showString " }"

instance Eq Context where
  (==)
    Context { vars = vars0, nativeVars = nativeVars0, pats = pats0, internalName = internalName0 }
    Context { vars = vars1, nativeVars = nativeVars1, pats = pats1, internalName = internalName1 } =
      (==)
        (vars0, Map.keys nativeVars0, pats0, internalName0)
        (vars1, Map.keys nativeVars1, pats1, internalName1)

instance Default Context where
  def = Context mempty mempty mempty 0 Nothing

insertVar :: MonadState Context m => QName -> Expr -> m ()
insertVar name expr =
  state $ \ctx@Context { vars } ->
    ((), ctx { vars = Map.insert name expr vars })

insertNativeVar :: MonadState Context m => QName -> ReaderState Context Expr -> m ()
insertNativeVar name expr =
  state $ \ctx@Context { nativeVars } ->
    ((), ctx { nativeVars = Map.insert name expr nativeVars })

newName :: MonadState Context m => m Name
newName =
  state $ \ctx@Context { internalName } ->
    (Text.pack (show internalName) <> "thunk", ctx { internalName = internalName + 1 })

lookupVar :: (MonadFail m, MonadReader Context m) => QName -> m (Either Expr (ReaderState Context Expr))
lookupVar name = do
  Context { vars, nativeVars } <- ask
  case Map.lookup name vars of
    Just expr -> pure $ Left expr
    Nothing ->
      case Map.lookup name nativeVars of
        Just expr -> pure $ Right expr
        Nothing   -> fail $ "no such value variables: " ++ ppQName name

getVarAssocs :: MonadReader Context m => m [(QName, Either Expr (ReaderState Context Expr))]
getVarAssocs = do
  Context { vars, nativeVars } <- ask
  pure $ (second Left <$> Map.assocs vars) ++ (second Right <$> Map.assocs nativeVars)

insertPat :: MonadState Context m => QName -> Pat -> m ()
insertPat name pat =
  state $ \ctx@Context { pats } -> ((), ctx { pats = Map.insert name pat pats })

lookupPat :: (MonadFail m, MonadReader Context m) => QName -> m Pat
lookupPat name = do
  Context { pats } <- ask
  case Map.lookup name pats of
    Just a  -> pure a
    Nothing -> fail $ "no such pattern variables: " ++ ppQName name

ppQName :: QName -> String
ppQName (m, n)
  | Text.null m = Text.unpack n
  | otherwise = Text.unpack $ m <> "." <> n

type Namespace = Map Name Context

insertModule :: MonadState Namespace m => Name -> Context -> m ()
insertModule name ctx =
  state $ ((),) . Map.insert name ctx

lookupModule :: (MonadFail m, MonadReader Namespace m) => Name -> m Context
lookupModule mod = do
  ns <- ask
  case Map.lookup mod ns of
    Just ctx -> pure ctx
    Nothing  -> fail $ "no such modules: " ++ Text.unpack mod

memberModule :: MonadReader Namespace m => Name -> m Bool
memberModule mod = asks $ Map.member mod

data Qualified = Qualified deriving (Show, Eq)

data Import =
  Import Name (Maybe Qualified) (Maybe Name) [Name]
  deriving (Show, Eq)

import' :: (MonadFail m, MonadReader Namespace m, MonadState Context m) => Import -> m ()
import' (Import mod qual alias names) = do
  ctx <- lookupModule mod
  vs <-
    case names of
      [] -> (first snd <$>) <$> runReaderT getVarAssocs ctx
      _  -> sequence $ (\n -> (n,) <$> runReaderT (lookupVar ("", n)) ctx) <$> names
  let
    mods =
      case (qual, alias) of
        (Nothing, Nothing)        -> [""]
        (Nothing, Just a)         -> ["", a]
        (Just Qualified, Nothing) -> [mod]
        (Just Qualified, Just a)  -> [a]
  for_ vs $ \(n, v) ->
    for_ mods $ \m ->
      case v of
        Left expr  -> insertVar (m, n) expr
        Right expr -> insertNativeVar (m, n) expr
