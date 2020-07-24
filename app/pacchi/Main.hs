{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import qualified Pacch as P

import           Control.Monad                    (void)
import           Control.Monad.Reader             (MonadReader (ask), ReaderT (runReaderT))
import           Control.Monad.ReaderState.Strict (ReaderStateT (runReaderStateT))
import           Control.Monad.Result             (ResultT, catchError, runResultT)
import           Control.Monad.Trans.Except.Result             (ResultT (ResultT))
import           Control.Monad.State.Strict       (runStateT)
import           Control.Monad.Trans.Class        (lift)
import           Data.Default.Class               (Default (def))
import           Data.Functor                     (($>))
import           System.Console.Haskeline         (InputT, getInputLine, outputStrLn, runInputT)
import qualified System.Console.Haskeline         as L
import           System.Directory                 (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import           System.FilePath                  ((</>))
import qualified Text.Megaparsec                  as M
import qualified Text.Megaparsec.Char             as M
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception.Safe (catchAny, Exception (displayException), MonadThrow, MonadCatch)

main :: IO ()
main = do
  configDir <- getXdgDirectory XdgConfig "pacch"
  createDirectoryIfMissing True configDir
  let
    haskelineSettings :: L.Settings IO
    haskelineSettings =
      L.defaultSettings
        { L.historyFile = Just $ configDir </> "history" }
  void $ runInputT haskelineSettings $ runResultT (runStateT (runReaderStateT (runStateT (runReaderStateT repl) P.natives)) def)

repl :: ReaderStateT P.Namespace (ReaderStateT P.Context (ResultT (InputT IO))) ()
repl = do
  input <- lift $ lift $ lift $ getInputLine "pacchi> "
  case input of
    Nothing -> pure () -- Ctrl-D
    Just input ->
      do
        input <- P.parse (inputParser <* M.eof) "<repl>" $ T.pack input
        case input of
          Cmd Quit -> pure ()
          Cmd Help -> do
            lift $ lift $ lift $ outputStrLn helpMessage
            repl
          Cmd (Load path) -> do
            code <- liftIO $ T.readFile path
            module' <- P.parse (P.modParser <* M.eof) path code
            P.evalMod module'
            repl
          Cmd (Print var) -> do
            expr <- lift $ P.lookupVar var
            lift $ lift $ lift $ outputStrLn $ ((P.printQVar var ++ " = ") ++) $
              case expr of
                Left expr -> P.printExpr expr
                Right _ -> "(native value)"
            repl
          Import impt -> do
            ns <- ask
            lift $ runReaderT (P.import' impt) ns
            repl
          Decl decl -> do
            lift $ P.evalDecl decl
            repl
          Expr expr -> do
            expr <- lift $ P.evalExpr expr
            lift $ lift $ lift $ outputStrLn $ P.printExpr expr
            repl
      `catchError`
      (\e -> do
        printError e
        repl
      )
      `catchAny`
      (\e -> do
        printError $ displayException e
        repl
      )
  where
    printError :: String -> ReaderStateT P.Namespace (ReaderStateT P.Context (ResultT (InputT IO))) ()
    printError = lift . lift . lift . outputStrLn . ("error: " ++)

helpMessage :: String
helpMessage =
  ":help       show this message\n\
  \:quit       quit pacchi\n\
  \:load PATH  load a file\n\
  \:print VAR  print a value of a variable"

data Input
  = Cmd Cmd
  | Decl P.Decl
  | Expr P.Expr
  | Import P.Import

data Cmd
  = Quit
  | Help
  | Load FilePath
  | Print P.QName

inputParser :: P.Parser Input
inputParser =
  M.choice
    [ Cmd <$> M.try cmd
    , Import <$> M.try P.imptParser
    , Decl <$> M.try P.declParser
    , Expr <$> M.try P.exprParser
    ]

cmd :: P.Parser Cmd
cmd =
  M.choice
    [ M.try (M.string ":quit") $> Quit
    , M.try (M.string ":help") $> Help
    , M.try $ M.string ":load " >> Load <$> M.many M.anySingle
    , M.try $ M.string ":print " >> Print <$> P.qvarParser
    ]

deriving newtype instance MonadThrow m => MonadThrow (ResultT m)

deriving newtype instance MonadCatch m => MonadCatch (ResultT m)
