{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative ( optional, (<|>) )
import Control.Monad (void, when, foldM)
import Control.Monad.State.Strict ()
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void ( Void )
import System.Directory (findFile)
import System.Exit (die)
import System.FilePath (takeBaseName)
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
      getSourcePos,
      sourcePosPretty,
      SourcePos,
      Parsec,
      MonadParsec(try, notFollowedBy, label),
      ParseErrorBundle )
import Text.Megaparsec.Char
    ( char, alphaNumChar, letterChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

import Types (Model(..), Defs(..), BindName(..), BoundValue(..), Name)

type Parser = Parsec Void Text

spaceP :: Parser ()
spaceP =
  L.space
    space1
    (L.skipLineComment "\\*")
    (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

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

symbols :: [Text] -> Parser ()
symbols = mapM_ symbol

modelValueTagP :: Parser ()
modelValueTagP = try $ symbols ["[", "model", "value", "]"]

symmetryTagP :: Parser Bool
symmetryTagP = option False (True <$ symbols ["<", "symmetrical", ">"])

modelValueP :: Parser BoundValue
modelValueP = pure ModelValue

nameSetP :: Parser [Name]
nameSetP = between (symbol "{") (symbol "}") $ commaSep identifierP

modelValuesP :: Parser BoundValue
modelValuesP = ModelValues <$> symmetryTagP <*> nameSetP

boundValueP :: Parser () -> Parser BoundValue
boundValueP uptoP =
    (modelValueTagP >> (modelValuesP <|> modelValueP))
    <|>
    Expression <$> someTill anySingle (lookAhead uptoP)

bindP :: Parser (BindName, BoundValue)
bindP = do
    let lhs = bindNameP <* symbol "<-"
        uptoP = try (void lhs) <|> void (symbol ";")
    name <- lhs
    value <- boundValueP uptoP
    pure (name, value)

booleanP :: Parser Bool
booleanP = (True <$ symbol "TRUE") <|> (False <$ symbol "FALSE")

type Decls = [Decl]

data Decl
    = ModuleName SourcePos Name
    | Constants SourcePos [(BindName, BoundValue)]
    | Specification SourcePos Name
    | CheckDeadlock SourcePos Bool
    | Invariants SourcePos [Name]
    | Properties SourcePos [Name]
    deriving (Show, Eq)

data PreModel = PreModel
    { pm_moduleName :: Maybe Name
    , pm_constants :: [(BindName, BoundValue)]
    , pm_specification :: Maybe Name
    , pm_checkDeadlock :: Maybe Bool
    , pm_invariants :: [Name]
    , pm_properties :: [Name]
    }
    deriving (Show, Eq)

resolveError :: SourcePos -> String -> Either String a
resolveError pos msg = Left $ printf "%s: %s" (sourcePosPretty pos) msg

resolve' :: Decls -> Either String PreModel
resolve' = foldM f pre0
    where
        pre0 = PreModel Nothing [] Nothing Nothing [] []
        f pre decl = case decl of
            ModuleName pos name -> do
                when (isJust (pm_moduleName pre)) $ resolveError pos "multiple MODULE declaration"
                pure pre { pm_moduleName = Just name }
            Specification pos name -> do
                when (isJust (pm_specification pre)) $ resolveError pos "multiple SPECIFICATION declaration"
                pure pre { pm_specification = Just name }
            CheckDeadlock pos value -> do
                when (isJust (pm_checkDeadlock pre)) $ resolveError pos "multiple CHECK_DEADLOCK declaration"
                pure pre { pm_checkDeadlock = Just value }
            Constants _ defs ->
                let defs0 = pm_constants pre
                in pure pre { pm_constants = defs0 ++ defs }
            Properties _ props ->
                let props0 = pm_properties pre
                in pure pre { pm_properties = props0 ++ props }
            Invariants _ invs ->
                let invs0 = pm_invariants pre
                in pure pre { pm_invariants = invs0 ++ invs }

resolve :: String -> Decls -> Either String Model
resolve moduleNameDef decls = do
    PreModel {..} <- resolve' decls
    specificationName <- maybe (Left "SPECIFICATION must be specified") pure pm_specification
    let moduleName = fromMaybe moduleNameDef pm_moduleName
        checkDeadlock = fromMaybe True pm_checkDeadlock

    pure Model
        { m_module = moduleName
        , m_constants = Defs pm_constants
        , m_specification = specificationName
        , m_checkDeadlock = checkDeadlock
        , m_invariants = pm_invariants
        , m_properties = pm_properties
        }

declP :: Parser Decl
declP = do
    pos <- getSourcePos
    let
        moduleNameP = ModuleName pos <$> stmtP "MODULE" identifierP
        constantsP = Constants pos <$> stmtP "CONSTANTS" (many1 bindP)
        specP = Specification pos <$> stmtP "SPECIFICATION" identifierP
        deadlockP = CheckDeadlock pos <$> stmtP "CHECK_DEADLOCK" booleanP
        invsP = Invariants pos <$> stmtP "INVARIANT" (many1 identifierP)
        propsP = Properties pos <$> stmtP "PROPERTY" (many1 identifierP)

    moduleNameP <|> constantsP <|> specP <|> deadlockP <|> invsP <|> propsP

declsP :: Parser Decls
declsP = spaceP *> many1 declP

parseModel :: String -> Text -> Either String Decls
parseModel fileName content = case runParser declsP fileName content of
    Left es -> Left $ errorBundlePretty es
    Right ds -> Right ds

parseAndResolveModel :: String -> Text -> Either String Model
parseAndResolveModel fileName content = do
    decls <- parseModel fileName content
    resolve (takeBaseName fileName) decls

parseAndResolveModelFile :: [String] -> String -> IO Model
parseAndResolveModelFile paths fileName = do
    mfile <- findFile paths fileName

    let notFound = die $ printf "File `%s' not found in paths %s" fileName (show paths)

    file <- maybe notFound pure mfile

    content <- Text.readFile file

    case parseAndResolveModel file content of
        Left es -> die es
        Right model -> pure model
