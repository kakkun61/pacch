{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Pacch.Parse
  ( Parser
  , parse
  , mod
  , impt
  , decl
  , expr
  , qvar
  ) where

import Pacch.Data

import Prelude hiding (Integral (mod), Num (abs))

import           Control.Monad              (void)
import           Control.Monad.Combinators
import           Data.Bifunctor
import           Data.Char
import qualified Data.Either.Result         as Result
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import           Text.Megaparsec            hiding (match, parse)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Char       hiding (space, space1)
import qualified Text.Megaparsec.Char.Lexer as ML

{-# ANN module ("HLint: ignore Use <$>" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

type Parser = Parsec Void Text

parse :: MonadFail m => Parser a -> String -> Text -> m a
parse p n = Result.toMonadFail . Result.fromEither . first errorBundlePretty . M.parse p n

mod :: Parser Module
mod = do
  spaceLine
  name <-
    option "Main" $ line $ do
      void $ symbol "module"
      ns <- cons `sepBy` char '.'
      void $ symbol "where"
      pure $ mconcat $ intersperse "." ns
  impts <- many $ line impt
  decls <- many $ line decl
  void $ many horizontalSpace
  pure $ Module name impts decls

decl :: Parser Decl
decl =
  choice
    [ try varDecl
    , try dataDecl
    ]

varDecl :: Parser Decl
varDecl =
  do
    v <- var
    as <- many var
    void $ symbol "="
    e <- expr
    let e' = foldr (\n e -> Val $ Abs n e) e as
    pure $ VarDecl v e'
  <?> "varDecl"

dataDecl :: Parser Decl
dataDecl =
  do
    void $ symbol "data"
    typCon <- cons
    typVars <- many var
    void $ symbol "="
    valConss <-
      do
        valCon <- cons
        types <- many typ
        pure (valCon, types)
      `sepBy1`
      void (symbol "|")
    pure $ DataDecl typCon $ Data typVars valConss
  <?> "dataDecl"

expr :: Parser Expr
expr = foldl1 App <$> listExpr <?> "expr"

listExpr :: Parser [Expr]
listExpr =
  choice
    [ try $ do
        e <- elemExpr
        es <- listExpr
        pure $ e:es
    , try $ (:[]) <$> elemExpr
    ]
  <?> "listExpr"

elemExpr :: Parser Expr
elemExpr =
  choice
    [ try $ Val <$> val expr
    , try $ Var <$> qvar
    , try match
    , try stringLiteral
    , try $ paren expr
    ]
  <?> "elemExpr"

val :: Parser t -> Parser (Val t)
val t =
  choice
    [ try $ Int <$> lexeme ML.decimal
    , try $ Char <$> charLiteral
    , try $ abs t
    ]
  <?> "val"

cons :: Parser Name
cons =
  (<?> "cons") $
    lexeme $ do
      h <- upperChar
      t <- many identChar
      pure $ Text.pack $ h:t

var :: Parser Name
var =
  (<?> "var") $
    lexeme $ do
      e <-
        choice
          [ Left <$> keyword
          , (Right <$>) $ do
              h <- letterChar <|> char '_'
              t <- many identChar
              pure $ Text.pack $ h:t
          ]
      case e of
        Left k  -> fail $ "a keyword cannot be used as a variable: " ++ Text.unpack k
        Right v -> pure v

qvar :: Parser (Name, Name)
qvar =
  (<?> "qvar") $
    lexeme $ do
      vs <- var `sepBy1` char '.'
      case vs of
        [v] -> pure ("", v)
        _   -> pure (mconcat $ intersperse "." $ init vs, last vs)

identChar :: Parser Char
identChar = (<?> "identChar") $ satisfy $ or . sequenceA [isLetter, isNumber, (== '\'')]

-- TEMP
typ :: Parser Type
typ = var <|> ("(" <>) . (<> ")") . Text.pack <$> paren (some $ identChar <|> horizontalSpace) <?> "typ"

stringLiteral :: Parser Expr
stringLiteral =
  (<?> "stringLiteral") $
    lexeme $ do
      void $ char '"'
      s <- many $ satisfy (/= '"') -- TEMP
      void $ char '"'
      pure $ foldr (\c l -> Val $ Datum 1 [Val $ Char c, l]) (Val $ Datum 0 []) s

charLiteral :: Parser Prelude.Char
charLiteral =
  (<?> "charLiteral") $
    lexeme $ do
      void $ char '\''
      c <- anySingle -- TEMP
      void $ char '\''
      pure c

abs :: Parser t -> Parser (Val t)
abs t =
  do
    void $ symbol "\\"
    x <- var
    void $ symbol "->"
    b <- t
    pure $ Abs x b
  <?> "abs"

match :: Parser Expr
match =
  do
    void $ symbol "case"
    t <- expr
    void $ symbol "of"
    void $ symbol "{"
    bs <-
      do
        p <- pat
        void $ symbol "->"
        e <- expr
        pure (p, e)
      `sepBy1`
      void (symbol ";")
    space
    void $ symbol "}"
    pure $ Match t bs
  <?> "match"

pat :: Parser Pat
pat = foldl1 PApp <$> listPat <?> "pat"

listPat :: Parser [Pat]
listPat =
  choice
    [ try $ do
        e <- elemPat
        void $ some $ char ' '
        es <- listPat
        pure $ e:es
    , try $ (:[]) <$> elemPat
    ]
  <?> "listPat"

elemPat :: Parser Pat
elemPat =
  choice
    [ try $ PVal <$> val pat
    , try $ PVar <$> qvar
    , try $ paren pat
    ]
  <?> "elemPat"

keyword :: Parser Name
keyword =
  (<?> "keyword") $
    choice $
      try . symbol <$>
        [ "data"
        , "case"
        , "of"
        , "import"
        ]

isHorizontalSpace :: Char -> Bool
isHorizontalSpace = (||) <$> (== ' ') <*> (== '\t')

horizontalSpace :: Parser Char
horizontalSpace = satisfy isHorizontalSpace

paren :: Parser a -> Parser a
paren = (<?> "paren") . between (symbol "(") (symbol ")")

symbol :: Text -> Parser Text
symbol = (<?> "symbol") . ML.symbol space

lexeme :: Parser a -> Parser a
lexeme = (<?> "lexeme") . ML.lexeme space

space :: Parser ()
space =
  ML.space
    (void $ some horizontalSpace)
    skipLineComment
    skipBlockComment
  <?> "space"

skipLineComment :: Parser ()
skipLineComment = ML.skipLineComment "--" <?> "skipLineComment"

skipBlockComment :: Parser ()
skipBlockComment = ML.skipBlockCommentNested "{-" "-}" <?> "skipBlockComment"

spaceLine :: Parser ()
spaceLine =
  ML.space
    (void $ many horizontalSpace >> eol)
    skipLineComment
    skipBlockComment
  <?> "spaceLine"

line :: Parser a -> Parser a
line = (<?> "line") . ML.lexeme spaceLine

impt :: Parser Import
impt = do
  void $ symbol "import"
  q <- (const Qualified <$>) <$> optional (symbol "qualified")
  m <-
    lexeme $ do
      vs <- var `sepBy1` char '.'
      pure $ mconcat $ intersperse "." vs
  vs <- fromMaybe [] <$> optional (paren $ many var)
  a <-
    optional $ do
      void $ symbol "as"
      var
  pure $ Import m q a vs
