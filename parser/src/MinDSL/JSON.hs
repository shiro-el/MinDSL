{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | JSON serialization for MinDSL AST using Aeson
--
-- This module uses Generic deriving with shared options from MinDSL.Options
-- to ensure consistency between JSON output and TypeScript type generation.
module MinDSL.JSON
  ( -- * Encoding
    encodeScale
  , encodePretty
    -- * Re-exports
  , ToJSON(..)
  ) where

import Data.Aeson (ToJSON(..), genericToJSON, object, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import MinDSL.AST
import MinDSL.Options (minDSLOptions)

-- =============================================================================
-- Encoding Functions
-- =============================================================================

-- | Encode a Scale to JSON ByteString
encodeScale :: Scale -> BL.ByteString
encodeScale = encodePretty

-- =============================================================================
-- ToJSON Instances (Generic deriving with shared options)
-- =============================================================================

instance ToJSON SourcePos where
  toJSON = genericToJSON minDSLOptions

instance ToJSON SourceSpan where
  toJSON = genericToJSON minDSLOptions

instance ToJSON LocalizedText where
  toJSON = genericToJSON minDSLOptions

instance ToJSON LikertConfig where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Option where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Meta where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Item where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Subscale where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Section where
  toJSON = genericToJSON minDSLOptions

instance ToJSON Function where
  toJSON = genericToJSON minDSLOptions

-- =============================================================================
-- Custom ToJSON for types needing special handling
-- =============================================================================

-- | Scale needs a "type": "Scale" field for identification
instance ToJSON Scale where
  toJSON Scale{..} = object
    [ "type" .= ("Scale" :: Text)
    , "name" .= scaleName
    , "version" .= scaleVersion
    , "meta" .= scaleMeta
    , "responseType" .= scaleResponseType
    , "items" .= scaleItems
    , "subscales" .= scaleSubscales
    , "sections" .= scaleSections
    , "functions" .= scaleFunctions
    , "scoring" .= scaleScoring
    , "span" .= scaleSpan
    ]

-- | ResponseType uses custom tag names matching the TypeScript types
instance ToJSON ResponseType where
  toJSON (LikertResponse cfg) = object
    [ "type" .= ("likert" :: Text)
    , "config" .= cfg
    ]
  toJSON (OptionsResponse opts) = object
    [ "type" .= ("options" :: Text)
    , "options" .= opts
    ]
  toJSON FreeTextResponse = object
    [ "type" .= ("text" :: Text)
    ]
  toJSON (NumericResponse minVal maxVal) = object
    [ "type" .= ("numeric" :: Text)
    , "min" .= minVal
    , "max" .= maxVal
    ]

-- | Scoring has a custom structure with rules as array of {name, expression}
instance ToJSON Scoring where
  toJSON Scoring{..} = object
    [ "rules" .= map ruleToJSON scoringRules
    , "span" .= scoringSpan
    ]
    where
      ruleToJSON (name, expr) = object
        [ "name" .= name
        , "expression" .= expr
        ]

-- | Statement uses custom field names for clarity
instance ToJSON Statement where
  toJSON (ExprStmt expr) = object
    [ "type" .= ("ExprStmt" :: Text)
    , "expression" .= expr
    ]
  toJSON (AssignStmt name expr) = object
    [ "type" .= ("AssignStmt" :: Text)
    , "name" .= name
    , "expression" .= expr
    ]
  toJSON (IfStmt cond thenBranch elifs elseBranch) = object
    [ "type" .= ("IfStmt" :: Text)
    , "condition" .= cond
    , "then" .= thenBranch
    , "elif" .= map elifToJSON elifs
    , "else" .= elseBranch
    ]
    where
      elifToJSON (c, body) = object
        [ "condition" .= c
        , "body" .= body
        ]
  toJSON (ForStmt var iter body) = object
    [ "type" .= ("ForStmt" :: Text)
    , "variable" .= var
    , "iterable" .= iter
    , "body" .= body
    ]
  toJSON (ReturnStmt expr) = object
    [ "type" .= ("ReturnStmt" :: Text)
    , "expression" .= expr
    ]

-- | Expression uses custom field names matching TypeScript expectations
instance ToJSON Expression where
  toJSON (LiteralInt n) = object
    [ "type" .= ("LiteralInt" :: Text)
    , "value" .= n
    ]
  toJSON (LiteralFloat n) = object
    [ "type" .= ("LiteralFloat" :: Text)
    , "value" .= n
    ]
  toJSON (LiteralString s) = object
    [ "type" .= ("LiteralString" :: Text)
    , "value" .= s
    ]
  toJSON (LiteralBool b) = object
    [ "type" .= ("LiteralBool" :: Text)
    , "value" .= b
    ]
  toJSON LiteralNull = object
    [ "type" .= ("LiteralNull" :: Text)
    ]
  toJSON (Identifier name) = object
    [ "type" .= ("Identifier" :: Text)
    , "name" .= name
    ]
  toJSON (Range start end) = object
    [ "type" .= ("Range" :: Text)
    , "start" .= start
    , "end" .= end
    ]
  toJSON (Array elems) = object
    [ "type" .= ("Array" :: Text)
    , "elements" .= elems
    ]
  toJSON (Binary op left right) = object
    [ "type" .= ("Binary" :: Text)
    , "operator" .= op
    , "left" .= left
    , "right" .= right
    ]
  toJSON (Unary op expr) = object
    [ "type" .= ("Unary" :: Text)
    , "operator" .= op
    , "expression" .= expr
    ]
  toJSON (Call name args) = object
    [ "type" .= ("Call" :: Text)
    , "name" .= name
    , "arguments" .= args
    ]
  toJSON (Index expr idx) = object
    [ "type" .= ("Index" :: Text)
    , "expression" .= expr
    , "index" .= idx
    ]
  toJSON (Member expr name) = object
    [ "type" .= ("Member" :: Text)
    , "expression" .= expr
    , "member" .= name
    ]
  toJSON (Conditional cond thenExpr elseExpr) = object
    [ "type" .= ("Conditional" :: Text)
    , "condition" .= cond
    , "then" .= thenExpr
    , "else" .= elseExpr
    ]

-- | BinaryOp uses symbolic string representation for readability
instance ToJSON BinaryOp where
  toJSON Add = A.String "+"
  toJSON Sub = A.String "-"
  toJSON Mul = A.String "*"
  toJSON Div = A.String "/"
  toJSON Mod = A.String "%"
  toJSON Eq = A.String "=="
  toJSON Neq = A.String "!="
  toJSON Lt = A.String "<"
  toJSON Gt = A.String ">"
  toJSON Lte = A.String "<="
  toJSON Gte = A.String ">="
  toJSON And = A.String "and"
  toJSON Or = A.String "or"

-- | UnaryOp uses symbolic string representation for readability
instance ToJSON UnaryOp where
  toJSON Neg = A.String "-"
  toJSON Not = A.String "not"
