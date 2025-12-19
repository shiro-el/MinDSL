/**
 * MinDSL TypeScript Type Definitions
 *
 * AUTO-GENERATED from Haskell AST
 * DO NOT EDIT MANUALLY - Run 'mindsl-parser gen-types' to regenerate
 */

// =============================================================================
// Core Types (auto-generated from Haskell via aeson-typescript)
// =============================================================================

export type SourcePos = ISourcePos;

export interface ISourcePos {
  line: number;
  column: number;
  offset: number;
}

export type SourceSpan = ISourceSpan;

export interface ISourceSpan {
  start: SourcePos;
  end: SourcePos;
}

export type LocalizedText = ILocalizedText;

export interface ILocalizedText {
  ko: string | null;
  en: string | null;
}

export type LikertConfig = ILikertConfig;

export interface ILikertConfig {
  min: number;
  max: number;
  labels: LocalizedText[] | null;
  reverse: boolean;
}

export type Option = IOption;

export interface IOption {
  value: number;
  label: LocalizedText;
}

export type Meta = IMeta;

export interface IMeta {
  name: LocalizedText;
  description: LocalizedText | null;
  category: string | null;
  timeFrame: LocalizedText | null;
  authors: string[];
  citation: string | null;
  license: string | null;
}

export type Item = IItem;

export interface IItem {
  id: string;
  text: LocalizedText;
  responseType: ResponseType | null;
  required: boolean;
  subscale: string | null;
  reverse: boolean;
  condition: Expression | null;
  span: SourceSpan | null;
}

export type Subscale = ISubscale;

export interface ISubscale {
  name: string;
  label: LocalizedText;
  items: string[];
}

export type Section = ISection;

export interface ISection {
  name: string;
  label: LocalizedText;
  items: string[];
}

export type Function = IFunction;

export interface IFunction {
  name: string;
  params: string[];
  body: Statement[];
  span: SourceSpan | null;
}

// =============================================================================
// AST Types (matching custom ToJSON in MinDSL.JSON)
// =============================================================================

// ResponseType (custom ToJSON)
export type ResponseType =
  | { type: "likert"; config: LikertConfig }
  | { type: "options"; options: Option[] }
  | { type: "text" }
  | { type: "numeric"; min: number | null; max: number | null }

// BinaryOp (symbolic strings)
export type BinaryOp =
  | "+" | "-" | "*" | "/" | "%"
  | "==" | "!=" | "<" | ">" | "<=" | ">="
  | "and" | "or"

// UnaryOp (symbolic strings)
export type UnaryOp = "-" | "not"

// Expression AST
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

// ElifBranch helper
export interface ElifBranch {
  condition: Expression
  body: Statement[]
}

// Statement AST
export type Statement =
  | { type: "ExprStmt"; expression: Expression }
  | { type: "AssignStmt"; name: string; expression: Expression }
  | { type: "IfStmt"; condition: Expression; then: Statement[]; elif: ElifBranch[]; else: Statement[] | null }
  | { type: "ForStmt"; variable: string; iterable: Expression; body: Statement[] }
  | { type: "ReturnStmt"; expression: Expression }

// ScoringRule helper
export interface ScoringRule {
  name: string
  expression: Expression
}

// Scoring
export interface Scoring {
  rules: ScoringRule[]
  span: SourceSpan | null
}

// Scale (top-level)
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
export function getLocalizedText(text: ILocalizedText | null, lang: Language): string {
  if (!text) return ""
  return text[lang] ?? text.ko ?? text.en ?? ""
}
