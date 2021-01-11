{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
    ( parseTplFile
    ) where

import Control.Applicative ( optional, (<|>) )
import Control.Monad (void)
import Control.Monad.State.Strict ()
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void ( Void )
import System.Directory (findFile)
import Text.Megaparsec
    ( (<?>),
      anySingle,
      runParser,
      errorBundlePretty,
      between,
      many,
      sepBy,
      sepBy1,
      someTill,
      lookAhead,
      option,
      Parsec,
      MonadParsec(try, notFollowedBy, label),
      ParseErrorBundle )
import Text.Megaparsec.Char
    ( char, alphaNumChar, letterChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

import Types (Model(..), Defs(..), BindName(..), Name)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "\\*")
    (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` symbol ","

keywordP :: Text -> Parser ()
keywordP keyword = lexeme (string keyword <* notFollowedBy alphaNumChar) >> pure ()

identifierP :: Parser Name
identifierP = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '!') <?> "identifier")

stmtExP :: Parser () -> Parser a -> Parser a
stmtExP kwP = between (try kwP) (symbol ";")

stmtP :: Text -> Parser a -> Parser a
stmtP kw = stmtExP (keywordP kw)

bindNameP :: Parser BindName
bindNameP = do
    name <- identifierP
    args <- option [] $ between (symbol "(") (symbol ")") $ commaSep1 identifierP
    pure $ BindName name args

bindP :: Parser (BindName, String)
bindP = do
    let lhs = bindNameP <* symbol "<-"
    name <- lhs
    expr <- someTill anySingle $ lookAhead $ try (void lhs) <|> void (symbol ";")
    pure (name, expr)

booleanP :: Parser Bool
booleanP = (symbol "TRUE" *> pure True) <|> (symbol "FALSE" *> pure False)

modelP :: Parser Model
modelP = do
    moduleName :: Name <- stmtP "MODULE" identifierP
    modelValues <- option [] $ stmtExP (keywordP "MODEL" >> keywordP "VALUES")
        $ many1 identifierP
    constants <- option [] $ stmtP "CONSTANTS" $ many1 bindP
    specification <- stmtP "SPECIFICATION" identifierP
    checkDeadlock <- option True $ stmtP "CHECK_DEADLOCK" booleanP
    invariants <- option [] $ stmtP "INVARIANT" $ many1 identifierP
    properties <- option [] $ stmtP "PROPERTY" $ many1 identifierP

    pure Model
        { m_module = moduleName
        , m_specification = specification
        , m_checkDeadlock = checkDeadlock
        , m_invariants = invariants
        , m_properties = properties
        , m_constants = Defs modelValues constants }

parseTpl :: String -> Text -> Either (ParseErrorBundle Text Void) Model
parseTpl fileName content = runParser modelP fileName content

parseTplFile :: [String] -> String -> IO Model
parseTplFile paths fileName = do
    mfile <- findFile paths fileName

    let notFound = fail $ printf "File `%s' not found in paths %s" fileName (show paths)

    file <- maybe notFound pure mfile

    content <- Text.readFile file

    case parseTpl file content of
        Left es -> fail $ errorBundlePretty es
        Right model -> pure model
