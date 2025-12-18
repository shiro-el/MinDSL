{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | AST types for MinDSL (Minimal DSL for psychological assessments)
module MinDSL.AST
  ( -- * Main Types
    Scale(..)
  , Meta(..)
  , LocalizedText(..)
  , ResponseType(..)
  , LikertConfig(..)
  , Option(..)
  , Item(..)
  , Subscale(..)
  , Section(..)
  , Function(..)
  , Scoring(..)
  , Statement(..)
  , Expression(..)
  , BinaryOp(..)
  , UnaryOp(..)
    -- * Source Location
  , SourcePos(..)
  , SourceSpan(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- =============================================================================
-- Source Location
-- =============================================================================

-- | Source position (line, column)
data SourcePos = SourcePos
  { posLine   :: !Int
  , posColumn :: !Int
  , posOffset :: !Int
  } deriving (Show, Eq, Generic)

-- | Source span (start to end)
data SourceSpan = SourceSpan
  { spanStart :: !SourcePos
  , spanEnd   :: !SourcePos
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Scale Definition
-- =============================================================================

-- | Top-level scale definition
data Scale = Scale
  { scaleName        :: !Text
  , scaleVersion     :: !Text
  , scaleMeta        :: !Meta
  , scaleResponseType :: !ResponseType
  , scaleItems       :: ![Item]
  , scaleSubscales   :: ![Subscale]
  , scaleSections    :: ![Section]
  , scaleFunctions   :: ![Function]
  , scaleScoring     :: !Scoring
  , scaleSpan        :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Metadata
-- =============================================================================

-- | Scale metadata
data Meta = Meta
  { metaName        :: !LocalizedText
  , metaDescription :: !(Maybe LocalizedText)
  , metaCategory    :: !(Maybe Text)
  , metaTimeFrame   :: !(Maybe LocalizedText)  -- Time frame for the assessment
  , metaAuthors     :: ![Text]
  , metaCitation    :: !(Maybe Text)
  , metaLicense     :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- | Localized text with language variants
data LocalizedText = LocalizedText
  { textKo :: !(Maybe Text)
  , textEn :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Response Types
-- =============================================================================

-- | Response type specification
data ResponseType
  = LikertResponse !LikertConfig
  | OptionsResponse ![Option]
  | FreeTextResponse
  | NumericResponse !(Maybe Int) !(Maybe Int)  -- min, max
  deriving (Show, Eq, Generic)

-- | Likert scale configuration
data LikertConfig = LikertConfig
  { likertMin     :: !Int
  , likertMax     :: !Int
  , likertLabels  :: !(Maybe [LocalizedText])
  , likertReverse :: !Bool
  } deriving (Show, Eq, Generic)

-- | Option for choice-based responses
data Option = Option
  { optionValue :: !Int
  , optionLabel :: !LocalizedText
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Items
-- =============================================================================

-- | Assessment item (question)
data Item = Item
  { itemId           :: !Text
  , itemText         :: !LocalizedText
  , itemResponseType :: !(Maybe ResponseType)  -- Override scale default
  , itemRequired     :: !Bool
  , itemSubscale     :: !(Maybe Text)
  , itemReverse      :: !Bool
  , itemCondition    :: !(Maybe Expression)
  , itemSpan         :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Subscales & Sections
-- =============================================================================

-- | Subscale definition
data Subscale = Subscale
  { subscaleName  :: !Text
  , subscaleLabel :: !LocalizedText
  , subscaleItems :: ![Text]  -- Item IDs
  } deriving (Show, Eq, Generic)

-- | Section for grouping items
data Section = Section
  { sectionName  :: !Text
  , sectionLabel :: !LocalizedText
  , sectionItems :: ![Text]  -- Item IDs
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Functions
-- =============================================================================

-- | User-defined function
data Function = Function
  { funcName   :: !Text
  , funcParams :: ![Text]
  , funcBody   :: ![Statement]
  , funcSpan   :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Scoring
-- =============================================================================

-- | Scoring rules
data Scoring = Scoring
  { scoringRules :: ![(Text, Expression)]
  , scoringSpan  :: !(Maybe SourceSpan)
  } deriving (Show, Eq, Generic)

-- =============================================================================
-- Statements
-- =============================================================================

-- | Statement in function body
data Statement
  = ExprStmt !Expression
  | AssignStmt !Text !Expression
  | IfStmt !Expression ![Statement] ![(Expression, [Statement])] !(Maybe [Statement])
    -- ^ condition, then-branch, elif-branches, else-branch
  | ForStmt !Text !Expression ![Statement]
    -- ^ variable, iterable, body
  | ReturnStmt !Expression
  deriving (Show, Eq, Generic)

-- =============================================================================
-- Expressions
-- =============================================================================

-- | Expression AST
data Expression
  = LiteralInt !Int
  | LiteralFloat !Double
  | LiteralString !Text
  | LiteralBool !Bool
  | LiteralNull
  | Identifier !Text
  | Range !Text !Text          -- q1..q9
  | Array ![Expression]
  | Binary !BinaryOp !Expression !Expression
  | Unary !UnaryOp !Expression
  | Call !Text ![Expression]   -- function call
  | Index !Expression !Expression
  | Member !Expression !Text
  | Conditional !Expression !Expression !Expression  -- condition ? then : else
  deriving (Show, Eq, Generic)

-- | Binary operators
data BinaryOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  deriving (Show, Eq, Generic)

-- | Unary operators
data UnaryOp
  = Neg | Not
  deriving (Show, Eq, Generic)
