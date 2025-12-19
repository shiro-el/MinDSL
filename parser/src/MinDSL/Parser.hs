{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parser for MinDSL using Megaparsec
module MinDSL.Parser
  ( -- * Main Parser
    parseScale
  , parseScaleFile
    -- * Individual Parsers (exported for testing)
  , scaleP
  , metaP
  , itemP
  , expressionP
  , statementP
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import MinDSL.AST
import MinDSL.Lexer

-- =============================================================================
-- Main Entry Points
-- =============================================================================

-- | Parse a scale from text
parseScale :: Text -> Either (ParseErrorBundle Text Void) Scale
parseScale = parse (scn *> scaleP <* eof) "<input>"

-- | Parse a scale from file
parseScaleFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Scale)
parseScaleFile path = do
  content <- TIO.readFile path
  return $ parse (scn *> scaleP <* eof) path content

-- =============================================================================
-- Scale Parser
-- =============================================================================

scaleP :: Parser Scale
scaleP = do
  keyword "scale"
  void colon
  scaleName <- identifier
  void $ optional eol
  scn

  scaleVersion <- versionP
  scaleMeta <- metaP
  baseResponseType <- responseTypeP
  -- Parse optional standalone options section and merge with likert config
  maybeOptions <- optional optionsSectionP
  let scaleResponseType = mergeOptionsIntoResponseType baseResponseType maybeOptions
  scaleItems <- itemsP
  scaleSubscales <- fromMaybe [] <$> optional subscalesP
  scaleSections <- fromMaybe [] <$> optional sectionsP
  scaleFunctions <- fromMaybe [] <$> optional functionsP
  scaleScoring <- scoringP

  return Scale
    { scaleSpan = Nothing
    , ..
    }

-- | Merge standalone options into LikertResponse as labels
mergeOptionsIntoResponseType :: ResponseType -> Maybe [Option] -> ResponseType
mergeOptionsIntoResponseType rt Nothing = rt
mergeOptionsIntoResponseType (LikertResponse cfg) (Just opts) =
  LikertResponse cfg { likertLabels = Just (map optionLabel opts) }
mergeOptionsIntoResponseType rt _ = rt

-- | Version parser
versionP :: Parser Text
versionP = do
  keyword "version"
  void colon
  v <- stringLiteral <|> lexeme (T.pack <$> some (alphaNumChar <|> char '.'))
  void $ optional eol
  scn
  return v

-- =============================================================================
-- Metadata Parser
-- =============================================================================

metaP :: Parser Meta
metaP = do
  keyword "meta"
  void colon
  void $ optional eol
  scn

  -- Parse indented meta fields
  fields <- many $ try $ do
    notFollowedBy (keyword "response_type" <|> keyword "items")
    field <- metaFieldP
    void $ optional eol
    scn
    return field

  let metaName = findLocalizedText "name" fields
      metaDescription = Just $ findLocalizedText "description" fields
      metaCategory = findText "category" fields
      metaTimeFrame = findOptionalLocalizedText "time_frame" fields
      metaAuthors = findTextList "authors" fields
      metaCitation = findText "citation" fields
      metaLicense = findText "license" fields

  return Meta {..}

metaFieldP :: Parser (Text, MetaValue)
metaFieldP = do
  name <- identifier
  void colon
  value <- metaValueP
  return (name, value)

data MetaValue
  = MVText Text
  | MVLocalized LocalizedText
  | MVList [Text]
  deriving (Show)

metaValueP :: Parser MetaValue
metaValueP = choice
  [ try $ MVLocalized <$> localizedTextP
  , try $ MVList <$> textListP
  , MVText <$> (stringLiteral <|> identifier)
  ]

localizedTextP :: Parser LocalizedText
localizedTextP = do
  void lbrace
  pairs <- localizedPairP `sepBy` comma
  void rbrace
  let textKo = lookup "ko" pairs
      textEn = lookup "en" pairs
  return LocalizedText {..}

localizedPairP :: Parser (Text, Text)
localizedPairP = do
  lang <- identifier
  void colon
  text <- stringLiteral
  return (lang, text)

textListP :: Parser [Text]
textListP = do
  void lbracket
  items <- (stringLiteral <|> identifier) `sepBy` comma
  void rbracket
  return items

findLocalizedText :: Text -> [(Text, MetaValue)] -> LocalizedText
findLocalizedText key fields = case lookup key fields of
  Just (MVLocalized lt) -> lt
  Just (MVText t) -> LocalizedText (Just t) Nothing
  _ -> LocalizedText Nothing Nothing

findOptionalLocalizedText :: Text -> [(Text, MetaValue)] -> Maybe LocalizedText
findOptionalLocalizedText key fields = case lookup key fields of
  Just (MVLocalized lt) -> Just lt
  Just (MVText t) -> Just $ LocalizedText (Just t) Nothing
  _ -> Nothing

findText :: Text -> [(Text, MetaValue)] -> Maybe Text
findText key fields = case lookup key fields of
  Just (MVText t) -> Just t
  _ -> Nothing

findTextList :: Text -> [(Text, MetaValue)] -> [Text]
findTextList key fields = case lookup key fields of
  Just (MVList ts) -> ts
  _ -> []

-- =============================================================================
-- Response Type Parser
-- =============================================================================

responseTypeP :: Parser ResponseType
responseTypeP = do
  keyword "response_type"
  void colon
  rt <- responseTypeValueP
  void $ optional eol
  scn
  return rt

responseTypeValueP :: Parser ResponseType
responseTypeValueP = choice
  [ try likertResponseP
  , try optionsResponseP
  , try freeTextResponseP
  , numericResponseP
  ]

likertResponseP :: Parser ResponseType
likertResponseP = do
  void $ lexeme (string "likert")
  void lparen
  minVal <- integer
  void $ char '-'
  maxVal <- integer
  void rparen
  -- Optional labels
  labels <- optional $ do
    void comma
    keyword "labels"
    void colon
    void lbracket
    ls <- localizedTextP `sepBy` comma
    void rbracket
    return ls
  return $ LikertResponse LikertConfig
    { likertMin = minVal
    , likertMax = maxVal
    , likertLabels = labels
    , likertReverse = False
    }

optionsResponseP :: Parser ResponseType
optionsResponseP = do
  keyword "options"
  void colon
  void $ optional eol
  scn
  opts <- many $ try $ do
    void $ char '-'
    sc
    opt <- optionP
    void $ optional eol
    scn
    return opt
  return $ OptionsResponse opts

optionP :: Parser Option
optionP = do
  void lbrace
  -- Parse value: N, label: {...}
  void $ lexeme (string "value")
  void colon
  val <- integer
  void comma
  void $ lexeme (string "label")
  void colon
  lbl <- localizedTextP
  void rbrace
  return Option { optionValue = val, optionLabel = lbl }

-- | Parse standalone options section (for likert labels)
optionsSectionP :: Parser [Option]
optionsSectionP = do
  keyword "options"
  void colon
  void $ optional eol
  scn
  many $ try $ do
    void $ char '-'
    sc
    opt <- optionP
    void $ optional eol
    scn
    return opt

freeTextResponseP :: Parser ResponseType
freeTextResponseP = do
  void $ lexeme (string "text")
  return FreeTextResponse

numericResponseP :: Parser ResponseType
numericResponseP = do
  void $ lexeme (string "numeric")
  void lparen
  minVal <- optional integer
  void $ char '-'
  maxVal <- optional integer
  void rparen
  return $ NumericResponse minVal maxVal

-- =============================================================================
-- Items Parser
-- =============================================================================

itemsP :: Parser [Item]
itemsP = do
  keyword "items"
  void colon
  void $ optional eol
  scn
  many $ try $ do
    void $ char '-'
    sc
    item <- itemP
    void $ optional eol
    scn
    return item

itemP :: Parser Item
itemP = do
  void lbrace
  fields <- itemFieldP `sepBy` comma
  void rbrace

  let itemId = fromMaybe "" $ lookupItemField "id" fields
      itemText = fromMaybe (LocalizedText Nothing Nothing) $ lookupItemLocalizedText "text" fields
      itemResponseType = Nothing  -- TODO: parse override
      itemRequired = lookupItemBool "required" fields True
      itemSubscale = lookupItemField "subscale" fields
      itemReverse = lookupItemBool "reverse" fields False
      itemCondition = Nothing  -- TODO: parse condition
      itemSpan = Nothing

  return Item {..}

data ItemFieldValue
  = IFText Text
  | IFLocalized LocalizedText
  | IFBool Bool
  | IFExpr Expression
  deriving (Show)

itemFieldP :: Parser (Text, ItemFieldValue)
itemFieldP = do
  name <- identifier
  void colon
  value <- itemFieldValueP name
  return (name, value)

itemFieldValueP :: Text -> Parser ItemFieldValue
itemFieldValueP name
  | name == "text" = IFLocalized <$> localizedTextP
  | name == "required" || name == "reverse" = IFBool <$> boolP
  | name == "condition" = IFExpr <$> expressionP
  | otherwise = IFText <$> (stringLiteral <|> identifier)

boolP :: Parser Bool
boolP = (True <$ keyword "true") <|> (False <$ keyword "false")

lookupItemField :: Text -> [(Text, ItemFieldValue)] -> Maybe Text
lookupItemField key fields = case lookup key fields of
  Just (IFText t) -> Just t
  _ -> Nothing

lookupItemLocalizedText :: Text -> [(Text, ItemFieldValue)] -> Maybe LocalizedText
lookupItemLocalizedText key fields = case lookup key fields of
  Just (IFLocalized lt) -> Just lt
  _ -> Nothing

lookupItemBool :: Text -> [(Text, ItemFieldValue)] -> Bool -> Bool
lookupItemBool key fields def = case lookup key fields of
  Just (IFBool b) -> b
  _ -> def

-- =============================================================================
-- Subscales Parser
-- =============================================================================

subscalesP :: Parser [Subscale]
subscalesP = do
  keyword "subscales"
  void colon
  void $ optional eol
  scn
  many $ try subscaleP

subscaleP :: Parser Subscale
subscaleP = do
  name <- identifier
  void colon
  void $ optional eol
  sc

  -- Parse label and items
  void $ lexeme (string "label")
  void colon
  lbl <- localizedTextP
  void $ optional eol
  scn

  void $ lexeme (string "items")
  void colon
  items <- itemRangeOrListP
  void $ optional eol
  scn

  return Subscale
    { subscaleName = name
    , subscaleLabel = lbl
    , subscaleItems = items
    }

itemRangeOrListP :: Parser [Text]
itemRangeOrListP = choice
  [ try itemRangeP
  , itemListP
  ]

itemRangeP :: Parser [Text]
itemRangeP = do
  void lbracket
  start <- identifier
  void dotdot
  end <- identifier
  void rbracket
  return $ expandRange start end

itemListP :: Parser [Text]
itemListP = do
  void lbracket
  items <- identifier `sepBy` comma
  void rbracket
  return items

-- | Expand a range like q1..q9 to [q1, q2, ..., q9]
expandRange :: Text -> Text -> [Text]
expandRange start end =
  let prefix1 = T.takeWhile (not . isDigitChar) start
      prefix2 = T.takeWhile (not . isDigitChar) end
      num1 = read $ T.unpack $ T.dropWhile (not . isDigitChar) start :: Int
      num2 = read $ T.unpack $ T.dropWhile (not . isDigitChar) end :: Int
  in if prefix1 == prefix2
     then map (\n -> prefix1 <> T.pack (show n)) [num1..num2]
     else [start, end]  -- Fallback if prefixes don't match
  where
    isDigitChar c = c >= '0' && c <= '9'

-- =============================================================================
-- Sections Parser
-- =============================================================================

sectionsP :: Parser [Section]
sectionsP = do
  keyword "sections"
  void colon
  void $ optional eol
  scn
  many $ try sectionP

sectionP :: Parser Section
sectionP = do
  name <- identifier
  void colon
  void $ optional eol
  sc

  void $ lexeme (string "label")
  void colon
  lbl <- localizedTextP
  void $ optional eol
  scn

  void $ lexeme (string "items")
  void colon
  items <- itemRangeOrListP
  void $ optional eol
  scn

  return Section
    { sectionName = name
    , sectionLabel = lbl
    , sectionItems = items
    }

-- =============================================================================
-- Functions Parser
-- =============================================================================

functionsP :: Parser [Function]
functionsP = do
  keyword "functions"
  void colon
  void $ optional eol
  scn
  many $ try functionP

functionP :: Parser Function
functionP = do
  keyword "fn"
  name <- identifier
  void lparen
  params <- identifier `sepBy` comma
  void rparen
  void colon
  void $ optional eol
  scn
  body <- many $ try $ do
    stmt <- statementP
    void $ optional eol
    scn
    return stmt
  return Function
    { funcName = name
    , funcParams = params
    , funcBody = body
    , funcSpan = Nothing
    }

-- =============================================================================
-- Scoring Parser
-- =============================================================================

scoringP :: Parser Scoring
scoringP = do
  keyword "scoring"
  void colon
  void $ optional eol
  scn
  rules <- many $ try scoringRuleP
  return Scoring
    { scoringRules = rules
    , scoringSpan = Nothing
    }

scoringRuleP :: Parser (Text, Expression)
scoringRuleP = do
  name <- identifier
  void colon
  expr <- expressionP
  void $ optional eol
  scn
  return (name, expr)

-- =============================================================================
-- Statement Parser
-- =============================================================================

statementP :: Parser Statement
statementP = choice
  [ try returnStmtP
  , try ifStmtP
  , try forStmtP
  , try assignStmtP
  , exprStmtP
  ]

returnStmtP :: Parser Statement
returnStmtP = do
  keyword "return"
  expr <- expressionP
  return $ ReturnStmt expr

ifStmtP :: Parser Statement
ifStmtP = do
  keyword "if"
  cond <- expressionP
  void colon
  void $ optional eol
  scn
  thenBody <- many $ try $ do
    notFollowedBy (keyword "elif" <|> keyword "else")
    stmt <- statementP
    void $ optional eol
    scn
    return stmt

  -- Parse elif branches
  elifs <- many $ try $ do
    keyword "elif"
    elifCond <- expressionP
    void colon
    void $ optional eol
    scn
    elifBody <- many $ try $ do
      notFollowedBy (keyword "elif" <|> keyword "else")
      stmt <- statementP
      void $ optional eol
      scn
      return stmt
    return (elifCond, elifBody)

  -- Parse else branch
  elseBody <- optional $ do
    keyword "else"
    void colon
    void $ optional eol
    scn
    many $ try $ do
      stmt <- statementP
      void $ optional eol
      scn
      return stmt

  return $ IfStmt cond thenBody elifs elseBody

forStmtP :: Parser Statement
forStmtP = do
  keyword "for"
  var <- identifier
  keyword "in"
  iter <- expressionP
  void colon
  void $ optional eol
  scn
  body <- many $ try $ do
    stmt <- statementP
    void $ optional eol
    scn
    return stmt
  return $ ForStmt var iter body

assignStmtP :: Parser Statement
assignStmtP = do
  name <- identifier
  void $ symbol "="
  expr <- expressionP
  return $ AssignStmt name expr

exprStmtP :: Parser Statement
exprStmtP = ExprStmt <$> expressionP

-- =============================================================================
-- Expression Parser (with operator precedence)
-- =============================================================================

expressionP :: Parser Expression
expressionP = do
  expr <- makeExprParser termP operatorTable
  -- Ternary operator (lowest precedence, right-associative)
  optional (ternaryTail expr) >>= maybe (return expr) return
  where
    ternaryTail cond = do
      void $ symbol "?"
      thenExpr <- expressionP
      void $ symbol ":"
      elseExpr <- expressionP
      return $ Conditional cond thenExpr elseExpr

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ -- Highest precedence: unary operators
    [ Prefix (Unary Neg <$ symbol "-")
    , Prefix (Unary Not <$ keyword "not")
    ]
  , -- Multiplication, division, modulo
    [ InfixL (Binary Mul <$ symbol "*")
    , InfixL (Binary Div <$ symbol "/")
    , InfixL (Binary Mod <$ symbol "%")
    ]
  , -- Addition, subtraction
    [ InfixL (Binary Add <$ symbol "+")
    , InfixL (Binary Sub <$ symbol "-")
    ]
  , -- Comparison operators
    [ InfixN (Binary Eq <$ try (symbol "=="))
    , InfixN (Binary Neq <$ try (symbol "!="))
    , InfixN (Binary Lte <$ try (symbol "<="))
    , InfixN (Binary Gte <$ try (symbol ">="))
    , InfixN (Binary Lt <$ symbol "<")
    , InfixN (Binary Gt <$ symbol ">")
    ]
  , -- Logical AND
    [ InfixL (Binary And <$ keyword "and") ]
  , -- Logical OR (lowest precedence)
    [ InfixL (Binary Or <$ keyword "or") ]
  ]

termP :: Parser Expression
termP = do
  base <- atomP
  postfixOps base

postfixOps :: Expression -> Parser Expression
postfixOps expr = do
  next <- optional $ choice
    [ try $ do  -- Function call
        void lparen
        args <- expressionP `sepBy` comma
        void rparen
        case expr of
          Identifier name -> return $ Call name args
          _ -> fail "Expected function name"
    , try $ do  -- Index access
        void lbracket
        idx <- expressionP
        void rbracket
        return $ Index expr idx
    , try $ do  -- Member access
        void dot
        member <- identifier
        return $ Member expr member
    ]
  case next of
    Just e -> postfixOps e
    Nothing -> return expr

atomP :: Parser Expression
atomP = choice
  [ try $ do  -- Range expression: q1..q9
      start <- identifier
      void dotdot
      end <- identifier
      return $ Range start end
  , try callOrIdentP  -- Function call or identifier
  , try $ LiteralFloat <$> float
  , try $ LiteralInt <$> integer
  , LiteralString <$> stringLiteral
  , LiteralBool True <$ keyword "true"
  , LiteralBool False <$ keyword "false"
  , LiteralNull <$ keyword "null"
  , arrayP
  , parenP
  ]

callOrIdentP :: Parser Expression
callOrIdentP = do
  name <- identifier
  next <- optional $ do
    void lparen
    args <- expressionP `sepBy` comma
    void rparen
    return args
  case next of
    Just args -> return $ Call name args
    Nothing -> return $ Identifier name

arrayP :: Parser Expression
arrayP = do
  void lbracket
  elems <- expressionP `sepBy` comma
  void rbracket
  return $ Array elems

parenP :: Parser Expression
parenP = do
  void lparen
  expr <- expressionP
  void rparen
  return expr
