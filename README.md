# MinDSL

**심리 평가 도구를 위한 도메인 특화 언어**

MinDSL은 자기보고식 심리 평가 도구(설문지)를 정의하고 실행하기 위한 도메인 특화 언어(DSL)입니다. 하나의 `.mindsl` 파일로 Next.js 웹앱과 Python 분석 환경 모두를 지원합니다.

## 핵심 특징

- **YAML 기반 선언적 문법** - 배우기 쉽고 읽기 쉬운 구조
- **Python-like 표현식** - 익숙한 `and`, `or`, `==` 연산자
- **자동 역채점** - `reverse: true` 플래그로 자동 처리
- **하위척도 지원** - 문항 그룹핑 및 개별 채점
- **조건 분기** - Skip logic, 조건부 문항, 적응형 검사
- **다국어 내장** - 파일 내 다국어 텍스트 지원
- **튜링 완전** - 커스텀 함수로 어떤 채점 로직이든 표현 가능

## 빠른 시작

### PHQ-9 예제

```yaml
scale: PHQ-9
version: "1.0.0"

meta:
  name:
    ko: "환자 건강 질문지-9"
    en: "Patient Health Questionnaire-9"
  category: depression

response_type: likert
options:
  - { value: 0, label: { ko: "전혀 아니다", en: "Not at all" } }
  - { value: 1, label: { ko: "며칠 동안", en: "Several days" } }
  - { value: 2, label: { ko: "1주일 이상", en: "More than half the days" } }
  - { value: 3, label: { ko: "거의 매일", en: "Nearly every day" } }

items:
  - id: q1
    text:
      ko: "일을 하는 것에 흥미나 재미가 거의 없음"
      en: "Little interest or pleasure in doing things"
  - id: q2
    text:
      ko: "기분이 가라앉거나 우울하거나 희망이 없음"
      en: "Feeling down, depressed, or hopeless"
  # ... q3-q9

scoring:
  total: sum(all)
  severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])
```

## 설치

### 요구 사항

- [Stack](https://docs.haskellstack.org/) (Haskell 빌드 도구)
- Node.js 20+ (웹 런타임)
- Python 3.11+ (분석 런타임, 선택)

### 파서 빌드

```bash
# 빌드
make build

# 테스트
make test

# 전역 설치
make install
```

### 사용법

```bash
# .mindsl 파일을 JSON AST로 변환
make parse FILE=scales/depression/phq9.mindsl

# 개발 모드 (파일 감시)
make watch

# REPL
make repl
```

## 프로젝트 구조

```
MinDSL/
├── docs/                    # 문서
│   ├── OVERVIEW.md          # 개요
│   ├── SYNTAX.md            # 문법 명세
│   ├── BUILTINS.md          # 빌트인 함수
│   ├── EXAMPLES.md          # 예제 모음
│   └── ARCHITECTURE.md      # 아키텍처
│
├── scales/                  # DSL 파일들 (척도 정의)
│   ├── depression/          # 우울 척도
│   │   ├── phq9.mindsl
│   │   └── bdi2.mindsl
│   ├── anxiety/             # 불안 척도
│   │   ├── gad7.mindsl
│   │   └── stai.mindsl
│   └── stress/              # 스트레스 척도
│       └── pss10.mindsl
│
├── parser/                  # Haskell 파서
│   ├── app/Main.hs          # CLI 진입점
│   └── src/MinDSL/
│       ├── AST.hs           # AST 타입 정의
│       ├── Lexer.hs         # 렉서
│       ├── Parser.hs        # 파서 (Megaparsec)
│       └── JSON.hs          # JSON 직렬화 (Aeson)
│
├── next-app/                # Next.js 웹 앱
│
└── Makefile                 # 빌드 오케스트레이션
```

## 아키텍처

```
┌─────────────────┐
│  .mindsl 파일   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Haskell 파서   │  ← 빌드 타임
│  (Megaparsec)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│   JSON AST      │────▶│  TypeScript     │  ← 웹 런타임
│                 │     │  (Next.js)      │
└────────┬────────┘     └─────────────────┘
         │
         └─────────────▶┌─────────────────┐
                        │  Python         │  ← 분석 런타임
                        │  (pandas/numpy) │
                        └─────────────────┘
```

## 문서

| 문서 | 설명 |
|------|------|
| [개요](./docs/OVERVIEW.md) | MinDSL 소개 및 설계 철학 |
| [문법 명세](./docs/SYNTAX.md) | 전체 DSL 문법 참조 |
| [빌트인 함수](./docs/BUILTINS.md) | `sum`, `cut`, `count` 등 내장 함수 |
| [예제 모음](./docs/EXAMPLES.md) | PHQ-9, GAD-7, DASS-21 등 실제 척도 구현 |
| [아키텍처](./docs/ARCHITECTURE.md) | 시스템 설계 및 개발 가이드 |

## 지원 척도

현재 구현된 척도:

- **우울**: PHQ-9, BDI-II
- **불안**: GAD-7, STAI
- **스트레스**: PSS-10

## 표현식 언어

Python과 유사한 문법:

```yaml
scoring:
  total: sum(all)

  severity: |
    if total < 5:
      "minimal"
    elif total < 10:
      "mild"
    elif total < 15:
      "moderate"
    else:
      "severe"

  has_risk: q9 >= 1  # 자해 문항 체크
```

### 주요 연산자

| 연산자 | 설명 | 예시 |
|--------|------|------|
| `+`, `-`, `*`, `/` | 산술 | `total / 9` |
| `==`, `!=`, `<`, `>` | 비교 | `score >= 10` |
| `and`, `or`, `not` | 논리 | `q1 == 1 and q2 == 1` |

### 빌트인 함수

| 함수 | 설명 | 예시 |
|------|------|------|
| `sum()` | 합계 | `sum(all)`, `sum(q1..q9)` |
| `mean()` | 평균 | `mean(depression)` |
| `count()` | 조건 카운트 | `count(all, >= 2)` |
| `cut()` | 구간 분류 | `cut(total, [5, 10], [low, mid, high])` |

## 개발

```bash
# 파서 빌드 및 감시
make watch

# REPL에서 테스트
make repl
> :l MinDSL.Parser
> parseTest parseExpr "sum(q1..q9)"

# 테스트 실행
make test
```

## 라이선스

MIT License
