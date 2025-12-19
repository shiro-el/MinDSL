{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TypeScript type generation for MinDSL AST
--
-- This module generates TypeScript type definitions from Haskell AST types.
-- Types with generic ToJSON use aeson-typescript for automatic generation.
-- Types with custom ToJSON have manually written TypeScript definitions.
--
-- The generated types match the JSON output from MinDSL.JSON module.
module MinDSL.TypeScript
  ( -- * Generation
    generateTypeScriptFile
  ) where

import Data.Aeson.TypeScript.TH
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T

import MinDSL.AST
import MinDSL.Options (minDSLOptions)

-- =============================================================================
-- TypeScript Instances
-- Note: All types need instances for dependency resolution,
-- but only some are used in auto-generated output.
-- =============================================================================

-- Source Location types
$(deriveTypeScript minDSLOptions ''SourcePos)
$(deriveTypeScript minDSLOptions ''SourceSpan)

-- Localized text
$(deriveTypeScript minDSLOptions ''LocalizedText)

-- Response type components
$(deriveTypeScript minDSLOptions ''LikertConfig)
$(deriveTypeScript minDSLOptions ''Option)
$(deriveTypeScript minDSLOptions ''ResponseType)

-- Expression AST (needed for Item.condition reference)
$(deriveTypeScript minDSLOptions ''BinaryOp)
$(deriveTypeScript minDSLOptions ''UnaryOp)
$(deriveTypeScript minDSLOptions ''Expression)

-- Statement AST (needed for Function.body reference)
$(deriveTypeScript minDSLOptions ''Statement)

-- Scale components
$(deriveTypeScript minDSLOptions ''Item)
$(deriveTypeScript minDSLOptions ''Subscale)
$(deriveTypeScript minDSLOptions ''Section)
$(deriveTypeScript minDSLOptions ''Function)
$(deriveTypeScript minDSLOptions ''Scoring)
$(deriveTypeScript minDSLOptions ''Meta)
$(deriveTypeScript minDSLOptions ''Scale)

-- =============================================================================
-- Generation Functions
-- =============================================================================

-- | Get TypeScript declarations for generic types (with exports added)
genericTypeDeclarations :: Text
genericTypeDeclarations = addExports $ T.pack $ formatTSDeclarations $ mconcat
  [ getTypeScriptDeclarations (Proxy :: Proxy SourcePos)
  , getTypeScriptDeclarations (Proxy :: Proxy SourceSpan)
  , getTypeScriptDeclarations (Proxy :: Proxy LocalizedText)
  , getTypeScriptDeclarations (Proxy :: Proxy LikertConfig)
  , getTypeScriptDeclarations (Proxy :: Proxy Option)
  , getTypeScriptDeclarations (Proxy :: Proxy Meta)
  , getTypeScriptDeclarations (Proxy :: Proxy Item)
  , getTypeScriptDeclarations (Proxy :: Proxy Subscale)
  , getTypeScriptDeclarations (Proxy :: Proxy Section)
  , getTypeScriptDeclarations (Proxy :: Proxy Function)
  ]
  where
    -- Add export keyword to type and interface declarations
    addExports = T.replace "type " "export type " . T.replace "interface " "export interface "

-- | Custom TypeScript definitions for types with custom ToJSON
customTypeDefinitions :: Text
customTypeDefinitions = T.unlines
  [ "// ResponseType (custom ToJSON)"
  , "export type ResponseType ="
  , "  | { type: \"likert\"; config: LikertConfig }"
  , "  | { type: \"options\"; options: Option[] }"
  , "  | { type: \"text\" }"
  , "  | { type: \"numeric\"; min: number | null; max: number | null }"
  , ""
  , "// BinaryOp (symbolic strings)"
  , "export type BinaryOp ="
  , "  | \"+\" | \"-\" | \"*\" | \"/\" | \"%\""
  , "  | \"==\" | \"!=\" | \"<\" | \">\" | \"<=\" | \">=\""
  , "  | \"and\" | \"or\""
  , ""
  , "// UnaryOp (symbolic strings)"
  , "export type UnaryOp = \"-\" | \"not\""
  , ""
  , "// Expression AST"
  , "export type Expression ="
  , "  | { type: \"LiteralInt\"; value: number }"
  , "  | { type: \"LiteralFloat\"; value: number }"
  , "  | { type: \"LiteralString\"; value: string }"
  , "  | { type: \"LiteralBool\"; value: boolean }"
  , "  | { type: \"LiteralNull\" }"
  , "  | { type: \"Identifier\"; name: string }"
  , "  | { type: \"Range\"; start: string; end: string }"
  , "  | { type: \"Array\"; elements: Expression[] }"
  , "  | { type: \"Binary\"; operator: BinaryOp; left: Expression; right: Expression }"
  , "  | { type: \"Unary\"; operator: UnaryOp; expression: Expression }"
  , "  | { type: \"Call\"; name: string; arguments: Expression[] }"
  , "  | { type: \"Index\"; expression: Expression; index: Expression }"
  , "  | { type: \"Member\"; expression: Expression; member: string }"
  , "  | { type: \"Conditional\"; condition: Expression; then: Expression; else: Expression }"
  , ""
  , "// ElifBranch helper"
  , "export interface ElifBranch {"
  , "  condition: Expression"
  , "  body: Statement[]"
  , "}"
  , ""
  , "// Statement AST"
  , "export type Statement ="
  , "  | { type: \"ExprStmt\"; expression: Expression }"
  , "  | { type: \"AssignStmt\"; name: string; expression: Expression }"
  , "  | { type: \"IfStmt\"; condition: Expression; then: Statement[]; elif: ElifBranch[]; else: Statement[] | null }"
  , "  | { type: \"ForStmt\"; variable: string; iterable: Expression; body: Statement[] }"
  , "  | { type: \"ReturnStmt\"; expression: Expression }"
  , ""
  , "// ScoringRule helper"
  , "export interface ScoringRule {"
  , "  name: string"
  , "  expression: Expression"
  , "}"
  , ""
  , "// Scoring"
  , "export interface Scoring {"
  , "  rules: ScoringRule[]"
  , "  span: SourceSpan | null"
  , "}"
  , ""
  , "// Scale (top-level)"
  , "export interface Scale {"
  , "  type: \"Scale\""
  , "  name: string"
  , "  version: string"
  , "  meta: Meta"
  , "  responseType: ResponseType"
  , "  items: Item[]"
  , "  subscales: Subscale[]"
  , "  sections: Section[]"
  , "  functions: Function[]"
  , "  scoring: Scoring"
  , "  span: SourceSpan | null"
  , "}"
  ]

-- | Generate a complete TypeScript file with header and runtime types
generateTypeScriptFile :: Text
generateTypeScriptFile = T.unlines
  [ "/**"
  , " * MinDSL TypeScript Type Definitions"
  , " *"
  , " * AUTO-GENERATED from Haskell AST"
  , " * DO NOT EDIT MANUALLY - Run 'mindsl-parser gen-types' to regenerate"
  , " */"
  , ""
  , "// ============================================================================="
  , "// Core Types (auto-generated from Haskell via aeson-typescript)"
  , "// ============================================================================="
  , ""
  , genericTypeDeclarations
  , ""
  , "// ============================================================================="
  , "// AST Types (matching custom ToJSON in MinDSL.JSON)"
  , "// ============================================================================="
  , ""
  , customTypeDefinitions
  , ""
  , "// ============================================================================="
  , "// Runtime Types"
  , "// ============================================================================="
  , ""
  , "/** User responses keyed by item ID */"
  , "export type Responses = Record<string, number | string | null>"
  , ""
  , "/** Scoring results keyed by rule name */"
  , "export type ScoringResults = Record<string, number | string>"
  , ""
  , "/** Language preference */"
  , "export type Language = \"ko\" | \"en\""
  , ""
  , "/** Helper to get localized text */"
  , "export function getLocalizedText(text: ILocalizedText | null, lang: Language): string {"
  , "  if (!text) return \"\""
  , "  return text[lang] ?? text.ko ?? text.en ?? \"\""
  , "}"
  ]
