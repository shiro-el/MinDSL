/**
 * MinDSL TypeScript Type Definitions
 * Generated from parser/src/MinDSL/AST.hs JSON output
 */

// =============================================================================
// Core Types
// =============================================================================

export interface LocalizedText {
  ko: string | null
  en: string | null
}

export interface SourcePos {
  line: number
  column: number
  offset: number
}

export interface SourceSpan {
  start: SourcePos
  end: SourcePos
}

// =============================================================================
// Response Types
// =============================================================================

export interface LikertConfig {
  min: number
  max: number
  labels: LocalizedText[] | null
  reverse: boolean
}

export interface Option {
  value: number
  label: LocalizedText
}

export type ResponseType =
  | { type: "likert"; config: LikertConfig }
  | { type: "options"; options: Option[] }
  | { type: "text" }
  | { type: "numeric"; min: number | null; max: number | null }

// =============================================================================
// Expression AST
// =============================================================================

export type Expression =
  | { type: "LiteralInt"; value: number }
  | { type: "LiteralFloat"; value: number }
  | { type: "LiteralString"; value: string }
  | { type: "LiteralBool"; value: boolean }
  | { type: "LiteralNull" }
  | { type: "Identifier"; name: string }
  | { type: "Range"; start: string; end: string }
  | { type: "Array"; elements: Expression[] }
  | { type: "Binary"; operator: BinaryOp; left: Expression; right: Expression }
  | { type: "Unary"; operator: UnaryOp; expression: Expression }
  | { type: "Call"; name: string; arguments: Expression[] }
  | { type: "Index"; expression: Expression; index: Expression }
  | { type: "Member"; expression: Expression; member: string }
  | { type: "Conditional"; condition: Expression; then: Expression; else: Expression }

export type BinaryOp =
  | "+" | "-" | "*" | "/" | "%"
  | "==" | "!=" | "<" | ">" | "<=" | ">="
  | "and" | "or"

export type UnaryOp = "-" | "not"

// =============================================================================
// Statement AST
// =============================================================================

export type Statement =
  | { type: "ExprStmt"; expression: Expression }
  | { type: "AssignStmt"; name: string; expression: Expression }
  | { type: "IfStmt"; condition: Expression; then: Statement[]; elif: ElifBranch[]; else: Statement[] | null }
  | { type: "ForStmt"; variable: string; iterable: Expression; body: Statement[] }
  | { type: "ReturnStmt"; expression: Expression }

export interface ElifBranch {
  condition: Expression
  body: Statement[]
}

// =============================================================================
// Scale Components
// =============================================================================

export interface Item {
  id: string
  text: LocalizedText
  responseType: ResponseType | null
  required: boolean
  subscale: string | null
  reverse: boolean
  condition: Expression | null
  span: SourceSpan | null
}

export interface Subscale {
  name: string
  label: LocalizedText
  items: string[]
}

export interface Section {
  name: string
  label: LocalizedText
  items: string[]
}

export interface Function {
  name: string
  params: string[]
  body: Statement[]
  span: SourceSpan | null
}

export interface ScoringRule {
  name: string
  expression: Expression
}

export interface Scoring {
  rules: ScoringRule[]
  span: SourceSpan | null
}

export interface Meta {
  name: LocalizedText
  description: LocalizedText | null
  category: string | null
  timeFrame: LocalizedText | null
  authors: string[]
  citation: string | null
  license: string | null
}

// =============================================================================
// Top-level Scale
// =============================================================================

export interface Scale {
  type: "Scale"
  name: string
  version: string
  meta: Meta
  responseType: ResponseType
  items: Item[]
  subscales: Subscale[]
  sections: Section[]
  functions: Function[]
  scoring: Scoring
  span: SourceSpan | null
}

// =============================================================================
// Runtime Types
// =============================================================================

/** User responses keyed by item ID */
export type Responses = Record<string, number | string | null>

/** Scoring results keyed by rule name */
export type ScoringResults = Record<string, number | string>

/** Language preference */
export type Language = "ko" | "en"

/** Helper to get localized text */
export function getLocalizedText(text: LocalizedText | null, lang: Language): string {
  if (!text) return ""
  return text[lang] ?? text.ko ?? text.en ?? ""
}
