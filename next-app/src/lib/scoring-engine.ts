/**
 * MinDSL Scoring Engine
 * Evaluates Expression AST to compute scoring results
 */

import type {
  Expression,
  Scale,
  Item,
  Responses,
  ScoringResults,
} from "@/types/mindsl"

// =============================================================================
// Evaluation Context
// =============================================================================

interface EvalContext {
  responses: Responses
  items: Item[]
  computed: Record<string, number | string>
}

// =============================================================================
// Range Expansion
// =============================================================================

/**
 * Expand "q1..q9" into ["q1", "q2", ..., "q9"]
 */
function expandRange(start: string, end: string): string[] {
  const startMatch = start.match(/^([a-zA-Z_]+)(\d+)$/)
  const endMatch = end.match(/^([a-zA-Z_]+)(\d+)$/)

  if (!startMatch || !endMatch) {
    throw new Error(`Invalid range: ${start}..${end}`)
  }

  const [, startPrefix, startNum] = startMatch
  const [, endPrefix, endNum] = endMatch

  if (startPrefix !== endPrefix) {
    throw new Error(`Range prefix mismatch: ${startPrefix} vs ${endPrefix}`)
  }

  const from = parseInt(startNum, 10)
  const to = parseInt(endNum, 10)
  const result: string[] = []

  for (let i = from; i <= to; i++) {
    result.push(`${startPrefix}${i}`)
  }

  return result
}

// =============================================================================
// Builtin Functions
// =============================================================================

type BuiltinFn = (args: (number | string | number[] | string[])[], ctx: EvalContext) => number | string

const builtins: Record<string, BuiltinFn> = {
  /**
   * sum(items) - Sum all values
   */
  sum: (args, ctx) => {
    const values = flattenToNumbers(args[0], ctx)
    return values.reduce((a, b) => a + b, 0)
  },

  /**
   * mean(items) - Calculate average
   */
  mean: (args, ctx) => {
    const values = flattenToNumbers(args[0], ctx)
    if (values.length === 0) return 0
    return values.reduce((a, b) => a + b, 0) / values.length
  },

  /**
   * count(items, condition?) - Count items meeting condition
   */
  count: (args, ctx) => {
    const values = flattenToNumbers(args[0], ctx)
    if (args.length === 1) {
      return values.length
    }
    const threshold = args[1] as number
    return values.filter((v) => v >= threshold).length
  },

  /**
   * min(items) - Minimum value
   */
  min: (args, ctx) => {
    const values = flattenToNumbers(args[0], ctx)
    return Math.min(...values)
  },

  /**
   * max(items) - Maximum value
   */
  max: (args, ctx) => {
    const values = flattenToNumbers(args[0], ctx)
    return Math.max(...values)
  },

  /**
   * cut(value, thresholds, labels) - Categorize value into bins
   * Example: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])
   */
  cut: (args) => {
    const value = args[0] as number
    const thresholds = args[1] as number[]
    const labels = args[2] as string[]

    for (let i = 0; i < thresholds.length; i++) {
      if (value < thresholds[i]) {
        return labels[i]
      }
    }
    return labels[labels.length - 1]
  },

  /**
   * round(value, decimals?) - Round to decimals
   */
  round: (args) => {
    const value = args[0] as number
    const decimals = (args[1] as number) ?? 0
    const factor = Math.pow(10, decimals)
    return Math.round(value * factor) / factor
  },

  /**
   * floor(value) - Floor
   */
  floor: (args) => Math.floor(args[0] as number),

  /**
   * ceil(value) - Ceiling
   */
  ceil: (args) => Math.ceil(args[0] as number),

  /**
   * abs(value) - Absolute value
   */
  abs: (args) => Math.abs(args[0] as number),
}

/**
 * Flatten argument to array of numbers
 */
function flattenToNumbers(
  arg: number | string | number[] | string[],
  ctx: EvalContext
): number[] {
  if (Array.isArray(arg)) {
    return arg.map((item) => {
      if (typeof item === "number") return item
      // Item ID - look up response
      const response = ctx.responses[item]
      return typeof response === "number" ? response : 0
    })
  }
  if (typeof arg === "number") {
    return [arg]
  }
  // Single item ID
  const response = ctx.responses[arg]
  return [typeof response === "number" ? response : 0]
}

// =============================================================================
// Expression Evaluator
// =============================================================================

/**
 * Evaluate an Expression AST node
 */
function evaluate(expr: Expression, ctx: EvalContext): number | string | number[] | string[] {
  switch (expr.type) {
    case "LiteralInt":
    case "LiteralFloat":
      return expr.value

    case "LiteralString":
      return expr.value

    case "LiteralBool":
      return expr.value ? 1 : 0

    case "LiteralNull":
      return 0

    case "Identifier": {
      // Check computed values first
      if (expr.name in ctx.computed) {
        return ctx.computed[expr.name]
      }
      // Check if it's an item response
      if (expr.name in ctx.responses) {
        const response = ctx.responses[expr.name]
        return typeof response === "number" ? response : 0
      }
      // Could be a label identifier (for cut function)
      return expr.name
    }

    case "Range": {
      const ids = expandRange(expr.start, expr.end)
      return ids
    }

    case "Array": {
      const elements = expr.elements.map((el) => evaluate(el, ctx))
      // If all elements are numbers, return number[]
      if (elements.every((e) => typeof e === "number")) {
        return elements as number[]
      }
      // Otherwise return as string[] (for labels)
      return elements.map((e) => String(e))
    }

    case "Binary": {
      const left = evaluate(expr.left, ctx)
      const right = evaluate(expr.right, ctx)
      return evaluateBinary(expr.operator, left, right)
    }

    case "Unary": {
      const operand = evaluate(expr.expression, ctx)
      if (expr.operator === "-") {
        return -(operand as number)
      }
      if (expr.operator === "not") {
        return operand ? 0 : 1
      }
      return operand as number
    }

    case "Call": {
      const fn = builtins[expr.name]
      if (!fn) {
        throw new Error(`Unknown function: ${expr.name}`)
      }
      const args = expr.arguments.map((arg) => evaluate(arg, ctx))
      return fn(args as (number | string | number[])[], ctx)
    }

    case "Index": {
      const arr = evaluate(expr.expression, ctx)
      const idx = evaluate(expr.index, ctx) as number
      if (Array.isArray(arr)) {
        return arr[idx] as number | string
      }
      return 0
    }

    case "Member": {
      // For member access like item.subscale
      return 0
    }

    case "Conditional": {
      const cond = evaluate(expr.condition, ctx)
      if (cond) {
        return evaluate(expr.then, ctx) as number | string
      }
      return evaluate(expr.else, ctx) as number | string
    }

    default:
      return 0
  }
}

function evaluateBinary(
  op: string,
  left: number | string | number[] | string[],
  right: number | string | number[] | string[]
): number {
  const l = typeof left === "number" ? left : 0
  const r = typeof right === "number" ? right : 0

  switch (op) {
    case "+":
      return l + r
    case "-":
      return l - r
    case "*":
      return l * r
    case "/":
      return r !== 0 ? l / r : 0
    case "%":
      return r !== 0 ? l % r : 0
    case "==":
      return left === right ? 1 : 0
    case "!=":
      return left !== right ? 1 : 0
    case "<":
      return l < r ? 1 : 0
    case ">":
      return l > r ? 1 : 0
    case "<=":
      return l <= r ? 1 : 0
    case ">=":
      return l >= r ? 1 : 0
    case "and":
      return l && r ? 1 : 0
    case "or":
      return l || r ? 1 : 0
    default:
      return 0
  }
}

// =============================================================================
// Public API
// =============================================================================

/**
 * Calculate all scoring rules for a scale given responses
 */
export function calculateScores(
  scale: Scale,
  responses: Responses
): ScoringResults {
  const ctx: EvalContext = {
    responses,
    items: scale.items,
    computed: {},
  }

  const results: ScoringResults = {}

  // Process rules in order (later rules can reference earlier ones)
  for (const rule of scale.scoring.rules) {
    const value = evaluate(rule.expression, ctx)
    const finalValue = typeof value === "number" || typeof value === "string"
      ? value
      : Array.isArray(value) ? value[0] : 0

    results[rule.name] = finalValue as number | string
    ctx.computed[rule.name] = finalValue as number | string
  }

  return results
}

/**
 * Check if all required items have responses
 */
export function validateResponses(
  scale: Scale,
  responses: Responses
): { valid: boolean; missing: string[] } {
  const missing: string[] = []

  for (const item of scale.items) {
    if (item.required && (responses[item.id] === null || responses[item.id] === undefined)) {
      missing.push(item.id)
    }
  }

  return {
    valid: missing.length === 0,
    missing,
  }
}

/**
 * Apply reverse scoring to responses based on item configuration
 */
export function applyReverseScoring(
  scale: Scale,
  responses: Responses
): Responses {
  const result: Responses = { ...responses }

  // Get max value from response type
  let maxValue = 0
  if (scale.responseType.type === "likert") {
    maxValue = scale.responseType.config.max
  }

  for (const item of scale.items) {
    if (item.reverse && typeof result[item.id] === "number") {
      result[item.id] = maxValue - (result[item.id] as number)
    }
  }

  return result
}
