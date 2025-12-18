{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for MinDSL using Megaparsec
module MinDSL.Lexer
  ( -- * Parser Type
    Parser
    -- * Space Consumers
  , sc
  , scn
  , lexeme
  , symbol
    -- * Literals
  , integer
  , float
  , number
  , stringLiteral
  , multilineString
    -- * Identifiers & Keywords
  , identifier
  , keyword
  , reserved
    -- * Operators
  , operator
    -- * Punctuation
  , colon
  , comma
  , dot
  , dotdot
  , lparen
  , rparen
  , lbracket
  , rbracket
  , lbrace
  , rbrace
    -- * Indentation
  , indentBlock
  , nonIndented
  , L.IndentOpt(..)
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- =============================================================================
-- Parser Type
-- =============================================================================

-- | Parser type with Text input and no custom error component
type Parser = Parsec Void Text

-- =============================================================================
-- Space Consumers
-- =============================================================================

-- | Line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- | Space consumer (no newlines)
sc :: Parser ()
sc = L.space hspace1 lineComment empty

-- | Space consumer with newlines
scn :: Parser ()
scn = L.space space1 lineComment empty

-- | Lexeme wrapper (consumes trailing whitespace, no newlines)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- =============================================================================
-- Literals
-- =============================================================================

-- | Integer literal
integer :: Parser Int
integer = lexeme L.decimal

-- | Float literal
float :: Parser Double
float = lexeme L.float

-- | Number (int or float) - returns Either Int Double
number :: Parser (Either Int Double)
number = lexeme $ try (Right <$> L.float) <|> (Left <$> L.decimal)

-- | String literal (single or double quoted)
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  quote <- char '"' <|> char '\''
  content <- manyTill L.charLiteral (char quote)
  return $ T.pack content

-- | Multi-line string (pipe syntax)
multilineString :: Parser Text
multilineString = do
  void $ char '|'
  void eol
  -- Get the indentation of the first line
  baseIndent <- T.length <$> takeWhileP Nothing (== ' ')
  firstLine <- takeWhileP Nothing (/= '\n')
  void $ optional eol
  -- Parse subsequent lines with at least baseIndent spaces
  rest <- many $ try $ do
    indent <- takeWhileP Nothing (== ' ')
    if T.length indent >= baseIndent
      then do
        line <- takeWhileP Nothing (/= '\n')
        void $ optional eol
        return $ T.drop baseIndent indent <> line
      else fail "Dedented"
  return $ T.intercalate "\n" (firstLine : rest)

-- =============================================================================
-- Identifiers & Keywords
-- =============================================================================

-- | Reserved words
reservedWords :: [Text]
reservedWords =
  [ "scale", "version", "meta", "response_type", "options"
  , "items", "subscales", "sections", "functions", "scoring"
  , "if", "elif", "else", "for", "in", "return", "fn"
  , "and", "or", "not", "true", "false", "null"
  ]

-- | Identifier parser
identifier :: Parser Text
identifier = lexeme $ try $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if ident `elem` reservedWords
    then fail $ "Reserved word: " ++ T.unpack ident
    else return ident

-- | Keyword parser (case insensitive)
keyword :: Text -> Parser ()
keyword kw = lexeme $ try $ do
  void $ string' kw
  notFollowedBy alphaNumChar

-- | Check if a word is reserved
reserved :: Text -> Bool
reserved = (`elem` reservedWords)

-- =============================================================================
-- Operators
-- =============================================================================

-- | Operator parser
operator :: Text -> Parser Text
operator op = lexeme $ try $ string op <* notFollowedBy opChar
  where
    opChar = oneOf ("+-*/%=<>!&|" :: String)

-- =============================================================================
-- Punctuation
-- =============================================================================

colon :: Parser Text
colon = symbol ":"

comma :: Parser Text
comma = symbol ","

dot :: Parser Text
dot = symbol "." <* notFollowedBy (char '.')

dotdot :: Parser Text
dotdot = symbol ".."

lparen :: Parser Text
lparen = symbol "("

rparen :: Parser Text
rparen = symbol ")"

lbracket :: Parser Text
lbracket = symbol "["

rbracket :: Parser Text
rbracket = symbol "]"

lbrace :: Parser Text
lbrace = symbol "{"

rbrace :: Parser Text
rbrace = symbol "}"

-- =============================================================================
-- Indentation
-- =============================================================================

-- | Parse an indented block
indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

-- | Parse a non-indented item
nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn
