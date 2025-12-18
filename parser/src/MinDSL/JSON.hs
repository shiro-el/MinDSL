{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | JSON serialization for MinDSL AST using Aeson
module MinDSL.JSON
  ( -- * Encoding
    encodeScale
  , encodePretty
    -- * Re-exports
  , ToJSON(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import MinDSL.AST

-- =============================================================================
-- Encoding Functions
-- =============================================================================

-- | Encode a Scale to JSON ByteString
encodeScale :: Scale -> BL.ByteString
encodeScale = encodePretty

-- =============================================================================
-- ToJSON Instances
-- =============================================================================

instance ToJSON SourcePos where
  toJSON SourcePos{..} = object
    [ "line" .= posLine
    , "column" .= posColumn
    , "offset" .= posOffset
    ]

instance ToJSON SourceSpan where
  toJSON SourceSpan{..} = object
    [ "start" .= spanStart
    , "end" .= spanEnd
    ]

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

instance ToJSON Meta where
  toJSON Meta{..} = object
    [ "name" .= metaName
    , "description" .= metaDescription
    , "category" .= metaCategory
    , "timeFrame" .= metaTimeFrame
    , "authors" .= metaAuthors
    , "citation" .= metaCitation
    , "license" .= metaLicense
    ]

instance ToJSON LocalizedText where
  toJSON LocalizedText{..} = object
    [ "ko" .= textKo
    , "en" .= textEn
    ]

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

instance ToJSON LikertConfig where
  toJSON LikertConfig{..} = object
    [ "min" .= likertMin
    , "max" .= likertMax
    , "labels" .= likertLabels
    , "reverse" .= likertReverse
    ]

instance ToJSON Option where
  toJSON Option{..} = object
    [ "value" .= optionValue
    , "label" .= optionLabel
    ]

instance ToJSON Item where
  toJSON Item{..} = object
    [ "id" .= itemId
    , "text" .= itemText
    , "responseType" .= itemResponseType
    , "required" .= itemRequired
    , "subscale" .= itemSubscale
    , "reverse" .= itemReverse
    , "condition" .= itemCondition
    , "span" .= itemSpan
    ]

instance ToJSON Subscale where
  toJSON Subscale{..} = object
    [ "name" .= subscaleName
    , "label" .= subscaleLabel
    , "items" .= subscaleItems
    ]

instance ToJSON Section where
  toJSON Section{..} = object
    [ "name" .= sectionName
    , "label" .= sectionLabel
    , "items" .= sectionItems
    ]

instance ToJSON Function where
  toJSON Function{..} = object
    [ "name" .= funcName
    , "params" .= funcParams
    , "body" .= funcBody
    , "span" .= funcSpan
    ]

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

instance ToJSON UnaryOp where
  toJSON Neg = A.String "-"
  toJSON Not = A.String "not"
