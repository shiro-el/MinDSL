# MinDSL 아키텍처

## 설계 철학

**파서와 런타임의 분리**

- **파서 (Haskell)**: `.mindsl` → JSON AST 변환. 정적 분석, 유효성 검사
- **런타임 (TypeScript/Python)**: JSON AST를 소비하여 UI 렌더링 및 채점 실행

이 분리를 통해:
- 파서는 Haskell의 강력한 타입 시스템과 파서 콤비네이터 활용
- 런타임은 각 플랫폼에 최적화된 언어로 구현
- JSON이 중간 표현으로서 언어 간 브릿지 역할

---

## 프로젝트 구조

```
MinDSL/
├── docs/                     # 문서
│   ├── OVERVIEW.md
│   ├── SYNTAX.md
│   ├── BUILTINS.md
│   ├── EXAMPLES.md
│   └── ARCHITECTURE.md
│
├── scales/                   # DSL 파일들 (척도 정의)
│   ├── depression/
│   │   ├── phq9.mindsl
│   │   ├── ces-d.mindsl
│   │   └── gds15.mindsl
│   ├── anxiety/
│   │   ├── gad7.mindsl
│   │   └── dass21.mindsl
│   └── ...
│
├── parser/                   # Haskell 파서
│   ├── app/
│   │   └── Main.hs           # CLI 진입점
│   ├── src/
│   │   └── MinDSL/
│   │       ├── Lexer.hs      # 렉서
│   │       ├── Parser.hs     # 파서 (Megaparsec)
│   │       ├── AST.hs        # AST 타입 정의
│   │       ├── Validator.hs  # 유효성 검사
│   │       └── JSON.hs       # JSON 직렬화 (Aeson)
│   ├── test/
│   │   └── Spec.hs
│   ├── mindsl-parser.cabal
│   └── stack.yaml
│
├── packages/
│   ├── runtime-web/          # TypeScript/React 런타임
│   │   ├── src/
│   │   │   ├── types/        # AST 타입 (JSON 스키마 기반)
│   │   │   │   └── ast.ts
│   │   │   ├── components/   # React 컴포넌트
│   │   │   │   ├── Scale.tsx
│   │   │   │   ├── Item.tsx
│   │   │   │   └── Results.tsx
│   │   │   ├── hooks/
│   │   │   │   ├── useScale.ts
│   │   │   │   └── useScoring.ts
│   │   │   └── engine/       # 채점 엔진
│   │   │       ├── evaluator.ts
│   │   │       └── builtins.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   └── runtime-python/       # Python 런타임
│       ├── mindsl/
│       │   ├── __init__.py
│       │   ├── types.py      # AST 타입 (Pydantic)
│       │   ├── loader.py     # JSON AST 로더
│       │   ├── engine.py     # 채점 엔진
│       │   └── builtins.py   # 빌트인 함수
│       ├── pyproject.toml
│       └── tests/
│
├── src/                      # Next.js 앱
│   ├── app/
│   │   ├── page.tsx
│   │   ├── layout.tsx
│   │   └── scales/
│   │       └── [id]/
│   │           └── page.tsx
│   └── lib/
│       └── scales/           # 빌드 시 생성된 JSON AST
│           ├── phq9.json
│           └── gad7.json
│
├── .vscode/
│   └── settings.json         # .mindsl → 문법 하이라이팅
│
├── package.json
├── Makefile                  # 빌드 오케스트레이션
└── tsconfig.json
```

---

## 데이터 흐름

```
                    [빌드 타임]                         [런타임]

┌─────────────────┐
│  .mindsl 파일   │
│  (커스텀 DSL)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Haskell 파서   │
│  (Megaparsec)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│   JSON AST      │────▶│  TypeScript     │
│   (.json)       │     │  Runtime        │
└────────┬────────┘     └────────┬────────┘
         │                       │
         │              ┌────────▼────────┐
         │              │  Next.js App    │
         │              │  (React UI)     │
         │              └─────────────────┘
         │
         │              ┌─────────────────┐
         └─────────────▶│  Python         │
                        │  Runtime        │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │  pandas/numpy   │
                        │  분석 스크립트  │
                        └─────────────────┘
```

---

## 핵심 컴포넌트

### 1. Haskell 파서

Megaparsec과 Aeson을 사용한 타입 안전 파서입니다.

```haskell
-- parser/src/MinDSL/AST.hs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MinDSL.AST where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Scale = Scale
  { scaleId      :: Text
  , version      :: Text
  , meta         :: Maybe Meta
  , responseType :: Maybe ResponseType
  , options      :: Maybe [Option]
  , items        :: [Item]
  , subscales    :: Maybe (Map Text [Text])
  , sections     :: Maybe [Section]
  , functions    :: Maybe (Map Text FunctionDef)
  , scoring      :: Map Text Expr
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Expr
  = Literal Value
  | Identifier Text
  | Range Text Text           -- q1..q9
  | Array [Expr]
  | Binary BinOp Expr Expr
  | Unary UnaryOp Expr
  | Call Text [Expr] (Maybe (Map Text Expr))  -- name, args, namedArgs
  | Index Expr Expr
  | Member Expr Text
  | Conditional Expr Expr (Maybe Expr)
  | Block [Statement]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Item = Item
  { itemId   :: Text
  , text     :: LocalizedText
  , itemType :: Maybe ResponseType
  , reverse  :: Maybe Bool
  , itemOptions :: Maybe [Option]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ... 나머지 타입 정의
```

```haskell
-- parser/src/MinDSL/Parser.hs

module MinDSL.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import MinDSL.AST

type Parser = Parsec Void Text

parseScale :: Parser Scale
parseScale = do
  _ <- symbol "scale" >> symbol ":"
  sid <- identifier
  _ <- symbol "version" >> symbol ":"
  ver <- stringLiteral

  meta <- optional parseMeta
  respType <- optional parseResponseType
  opts <- optional parseOptions
  itms <- parseItems
  subs <- optional parseSubscales
  sects <- optional parseSections
  funcs <- optional parseFunctions
  scor <- parseScoring

  pure $ Scale sid ver meta respType opts itms subs sects funcs scor

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable
  where
    operatorTable =
      [ [ InfixL (Binary Mul <$ symbol "*")
        , InfixL (Binary Div <$ symbol "/")
        ]
      , [ InfixL (Binary Add <$ symbol "+")
        , InfixL (Binary Sub <$ symbol "-")
        ]
      , [ InfixN (Binary Eq <$ symbol "==")
        , InfixN (Binary Neq <$ symbol "!=")
        , InfixN (Binary Lt <$ symbol "<")
        , InfixN (Binary Gt <$ symbol ">")
        ]
      , [ InfixL (Binary And <$ symbol "and") ]
      , [ InfixL (Binary Or <$ symbol "or") ]
      ]
```

### 2. CLI 인터페이스

```haskell
-- parser/app/Main.hs

module Main where

import MinDSL.Parser (parseFile)
import MinDSL.Validator (validate)
import MinDSL.JSON (toJSON)
import Options.Applicative

data Command
  = Parse FilePath OutputFormat
  | Validate FilePath
  | Version

data OutputFormat = JSON | PrettyJSON | AST

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Parse path fmt -> do
      result <- parseFile path
      case result of
        Left err -> die $ "Parse error: " <> show err
        Right ast -> do
          validation <- validate ast
          case validation of
            Left errs -> die $ "Validation errors: " <> show errs
            Right _ -> outputAST fmt ast

    Validate path -> do
      result <- parseFile path
      -- ...

    Version -> putStrLn "mindsl-parser 0.1.0"
```

**CLI 사용법:**

```bash
# 파싱 + JSON 출력
mindsl parse scales/depression/phq9.mindsl -o phq9.json

# 유효성 검사만
mindsl validate scales/depression/phq9.mindsl

# Pretty JSON
mindsl parse scales/depression/phq9.mindsl --pretty

# stdin에서 읽기
cat phq9.mindsl | mindsl parse --stdin

# 여러 파일 일괄 처리
mindsl parse scales/**/*.mindsl --out-dir src/lib/scales/
```

### 3. JSON AST 스키마

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "MinDSL Scale AST",
  "type": "object",
  "required": ["scale", "version", "items", "scoring"],
  "properties": {
    "scale": { "type": "string" },
    "version": { "type": "string" },
    "meta": {
      "type": "object",
      "properties": {
        "name": { "$ref": "#/definitions/localizedText" },
        "category": { "type": "string" },
        "timeFrame": { "$ref": "#/definitions/localizedText" }
      }
    },
    "responseType": {
      "type": "object",
      "properties": {
        "kind": { "enum": ["likert", "binary", "number", "time", "text"] },
        "min": { "type": "integer" },
        "max": { "type": "integer" }
      }
    },
    "items": {
      "type": "array",
      "items": { "$ref": "#/definitions/item" }
    },
    "scoring": {
      "type": "object",
      "additionalProperties": { "$ref": "#/definitions/expr" }
    }
  },
  "definitions": {
    "localizedText": {
      "oneOf": [
        { "type": "string" },
        { "type": "object", "additionalProperties": { "type": "string" } }
      ]
    },
    "expr": {
      "type": "object",
      "required": ["type"],
      "properties": {
        "type": {
          "enum": ["literal", "identifier", "range", "array", "binary",
                   "unary", "call", "index", "member", "conditional", "block"]
        }
      }
    },
    "item": {
      "type": "object",
      "required": ["id", "text"],
      "properties": {
        "id": { "type": "string" },
        "text": { "$ref": "#/definitions/localizedText" },
        "reverse": { "type": "boolean" }
      }
    }
  }
}
```

### 4. TypeScript 런타임

JSON AST를 로드하고 채점합니다.

```typescript
// packages/runtime-web/src/types/ast.ts
// JSON 스키마에서 자동 생성 또는 수동 정의

export interface ScaleAST {
  scale: string;
  version: string;
  meta?: Meta;
  responseType?: ResponseType;
  options?: Option[];
  items: Item[];
  subscales?: Record<string, string[]>;
  sections?: Section[];
  functions?: Record<string, FunctionDef>;
  scoring: Record<string, Expr>;
}

export type Expr =
  | { type: 'literal'; value: number | string | boolean | null }
  | { type: 'identifier'; name: string }
  | { type: 'range'; start: string; end: string }
  | { type: 'array'; elements: Expr[] }
  | { type: 'binary'; operator: BinaryOp; left: Expr; right: Expr }
  | { type: 'unary'; operator: UnaryOp; operand: Expr }
  | { type: 'call'; callee: string; args: Expr[]; namedArgs?: Record<string, Expr> }
  | { type: 'index'; object: Expr; index: Expr }
  | { type: 'member'; object: Expr; property: string }
  | { type: 'conditional'; condition: Expr; then: Expr; else?: Expr }
  | { type: 'block'; statements: Statement[] };
```

```typescript
// packages/runtime-web/src/engine/evaluator.ts

import { ScaleAST, Expr } from '../types/ast';
import { builtins } from './builtins';

export class Evaluator {
  private context: Map<string, any>;

  constructor(
    private scale: ScaleAST,
    private responses: Record<string, any>
  ) {
    this.context = new Map();
    this.initContext();
  }

  evaluate(expr: Expr): any {
    switch (expr.type) {
      case 'literal':
        return expr.value;

      case 'identifier':
        return this.context.get(expr.name) ?? this.responses[expr.name];

      case 'range':
        return this.expandRange(expr.start, expr.end);

      case 'call':
        const fn = builtins[expr.callee] ?? this.scale.functions?.[expr.callee];
        const args = expr.args.map(arg => this.evaluate(arg));
        return fn(this, ...args);

      case 'binary':
        return this.evaluateBinary(expr);

      // ... 나머지 케이스
    }
  }

  score(): Record<string, any> {
    const results: Record<string, any> = {};

    for (const [name, expr] of Object.entries(this.scale.scoring)) {
      results[name] = this.evaluate(expr);
      this.context.set(name, results[name]); // 이후 표현식에서 참조 가능
    }

    return results;
  }
}
```

### 5. Python 런타임

```python
# packages/runtime-python/mindsl/types.py

from typing import Any, Literal, Union
from pydantic import BaseModel

class LiteralExpr(BaseModel):
    type: Literal["literal"]
    value: Union[int, float, str, bool, None]

class IdentifierExpr(BaseModel):
    type: Literal["identifier"]
    name: str

class RangeExpr(BaseModel):
    type: Literal["range"]
    start: str
    end: str

class CallExpr(BaseModel):
    type: Literal["call"]
    callee: str
    args: list["Expr"]
    named_args: dict[str, "Expr"] | None = None

# ... Union 타입
Expr = Union[LiteralExpr, IdentifierExpr, RangeExpr, CallExpr, ...]

class Scale(BaseModel):
    scale: str
    version: str
    items: list[Item]
    scoring: dict[str, Expr]
    # ...
```

```python
# packages/runtime-python/mindsl/engine.py

from .types import Scale, Expr
from .builtins import BUILTINS

class Evaluator:
    def __init__(self, scale: Scale, responses: dict[str, Any]):
        self.scale = scale
        self.responses = responses
        self.context: dict[str, Any] = {}

    def evaluate(self, expr: Expr) -> Any:
        match expr:
            case {"type": "literal", "value": v}:
                return v

            case {"type": "identifier", "name": n}:
                return self.context.get(n) or self.responses.get(n)

            case {"type": "range", "start": s, "end": e}:
                return self._expand_range(s, e)

            case {"type": "call", "callee": fn, "args": args}:
                func = BUILTINS[fn]
                evaluated_args = [self.evaluate(a) for a in args]
                return func(self, *evaluated_args)

            # ...

    def score(self) -> dict[str, Any]:
        results = {}
        for name, expr in self.scale.scoring.items():
            results[name] = self.evaluate(expr)
            self.context[name] = results[name]
        return results
```

---

## 빌드 파이프라인

### Makefile

```makefile
.PHONY: all parser scales web clean

all: parser scales web

# Haskell 파서 빌드
parser:
	cd parser && stack build

# .mindsl → JSON 변환
scales: parser
	./parser/.stack-work/install/.../bin/mindsl-parser \
		parse scales/**/*.mindsl \
		--out-dir src/lib/scales/

# Next.js 빌드
web: scales
	npm run build

# 개발 모드
dev: scales
	npm run dev

# 파서만 테스트
test-parser:
	cd parser && stack test

# 전체 테스트
test: test-parser
	npm run test

clean:
	cd parser && stack clean
	rm -rf src/lib/scales/*.json
	rm -rf .next
```

### GitHub Actions

```yaml
# .github/workflows/ci.yml

name: CI

on: [push, pull_request]

jobs:
  parser:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.6'
      - run: cd parser && stack build
      - run: cd parser && stack test

  web:
    needs: parser
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
      - run: cd parser && stack build
      - run: make scales
      - run: npm ci
      - run: npm run build
      - run: npm test
```

---

## 개발 워크플로우

### 새 척도 추가

```bash
# 1. .mindsl 파일 작성
vim scales/anxiety/gad7.mindsl

# 2. 파싱 + 유효성 검사
mindsl validate scales/anxiety/gad7.mindsl

# 3. JSON 생성
mindsl parse scales/anxiety/gad7.mindsl -o src/lib/scales/gad7.json

# 4. 또는 전체 빌드
make scales
```

### 파서 개발

```bash
cd parser

# REPL에서 테스트
stack ghci
> :l MinDSL.Parser
> parseTest parseScale "scale: PHQ9\nversion: \"1.0\"\n..."

# 테스트 실행
stack test

# 빌드
stack build
```

---

## 테스트 전략

### Haskell 파서 테스트

```haskell
-- parser/test/ParserSpec.hs

module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import MinDSL.Parser
import MinDSL.AST

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses simple sum" $ do
      parse parseExpr "" "sum(q1..q9)" `shouldParse`
        Call "sum" [Range "q1" "q9"] Nothing

    it "parses cut function" $ do
      parse parseExpr "" "cut(total, [5, 10], [low, high])" `shouldParse`
        Call "cut"
          [ Identifier "total"
          , Array [Literal (Number 5), Literal (Number 10)]
          , Array [Identifier "low", Identifier "high"]
          ] Nothing

  describe "parseScale" $ do
    it "parses PHQ-9" $ do
      input <- readFile "test/fixtures/phq9.mindsl"
      case parse parseScale "" input of
        Left err -> expectationFailure (show err)
        Right scale -> do
          scaleId scale `shouldBe` "PHQ9"
          length (items scale) `shouldBe` 9
```

### TypeScript 런타임 테스트

```typescript
// packages/runtime-web/__tests__/evaluator.test.ts

import { Evaluator } from '../src/engine/evaluator';
import phq9 from '../../../src/lib/scales/phq9.json';

describe('Evaluator', () => {
  it('calculates PHQ-9 total', () => {
    const responses = {
      q1: 2, q2: 1, q3: 3, q4: 2,
      q5: 1, q6: 2, q7: 1, q8: 0, q9: 0
    };

    const evaluator = new Evaluator(phq9, responses);
    const results = evaluator.score();

    expect(results.total).toBe(12);
    expect(results.severity).toBe('moderate');
  });
});
```

### Python 런타임 테스트

```python
# packages/runtime-python/tests/test_engine.py

import pytest
from mindsl import load_scale, Evaluator

def test_phq9_scoring():
    scale = load_scale("src/lib/scales/phq9.json")
    responses = {
        "q1": 2, "q2": 1, "q3": 3, "q4": 2,
        "q5": 1, "q6": 2, "q7": 1, "q8": 0, "q9": 0
    }

    evaluator = Evaluator(scale, responses)
    results = evaluator.score()

    assert results["total"] == 12
    assert results["severity"] == "moderate"
```

---

## 향후 확장

### WASM 빌드 (선택적)

Haskell 파서를 WASM으로 컴파일하면 브라우저에서 직접 `.mindsl` 파일을 파싱할 수 있습니다.

```bash
# ghc-wasm 사용 (실험적)
wasm32-wasi-ghc -o mindsl-parser.wasm app/Main.hs
```

### Language Server Protocol (LSP)

VS Code에서 실시간 오류 표시, 자동완성 등을 지원하는 LSP 서버 구현.

```haskell
-- parser/src/MinDSL/LSP.hs
-- haskell-lsp 라이브러리 사용
```

### JSON Schema 자동 생성

Haskell AST 타입에서 JSON Schema를 자동 생성하여 TypeScript/Python 타입과 동기화.

```haskell
-- aeson-schemas 또는 openapi3 라이브러리 사용
```
