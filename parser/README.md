# MinDSL Parser

Haskell parser for MinDSL (Minimal DSL for psychological assessments).

## Overview

This parser reads `.mindsl` files and outputs JSON AST that can be consumed by TypeScript/Python runtimes.

## Building

```bash
# Build the project
stack build

# Run tests
stack test

# Install the CLI tool
stack install
```

## Usage

```bash
# Parse a .mindsl file and output JSON
mindsl-parser -i scale.mindsl -o scale.json

# Read from stdin, write to stdout
cat scale.mindsl | mindsl-parser

# Compact JSON output
mindsl-parser -i scale.mindsl -c
```

## Example

Input (`phq9.mindsl`):
```yaml
scale: PHQ9
version: "1.0.0"

meta:
  name: { ko: "환자 건강 설문지-9", en: "Patient Health Questionnaire-9" }
  category: depression

response_type: likert(0-3)

items:
  - { id: q1, text: { ko: "일을 하는 것에 흥미나 재미가 거의 없음", en: "Little interest or pleasure" } }
  - { id: q2, text: { ko: "기분이 가라앉거나 우울하거나 희망이 없음", en: "Feeling down, depressed" } }

scoring:
  total: sum(q1..q2)
  severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])
```

Output (JSON AST):
```json
{
  "type": "Scale",
  "name": "PHQ9",
  "version": "1.0.0",
  "meta": {
    "name": { "ko": "환자 건강 설문지-9", "en": "Patient Health Questionnaire-9" },
    "category": "depression"
  },
  "responseType": {
    "type": "likert",
    "config": { "min": 0, "max": 3 }
  },
  "items": [...],
  "scoring": {
    "rules": [
      { "name": "total", "expression": { "type": "Call", "name": "sum", ... } }
    ]
  }
}
```

## Project Structure

```
parser/
├── app/
│   └── Main.hs           # CLI entry point
├── src/
│   └── MinDSL/
│       ├── AST.hs        # AST type definitions
│       ├── Lexer.hs      # Lexer (tokenizer)
│       ├── Parser.hs     # Megaparsec parser
│       └── JSON.hs       # JSON serialization
├── test/
│   └── Spec.hs           # Test suite
├── package.yaml          # Stack package config
└── stack.yaml            # Stack resolver config
```

## Dependencies

- **megaparsec**: Parser combinator library
- **aeson**: JSON encoding/decoding
- **optparse-applicative**: CLI argument parsing
- **hspec**: Testing framework
