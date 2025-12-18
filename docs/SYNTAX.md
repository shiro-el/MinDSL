# MinDSL 문법 명세

## 파일 형식

- 확장자: `.mindsl`
- 기반: YAML 1.2
- 인코딩: UTF-8

### 에디터 설정

VSCode에서 YAML 하이라이팅을 위해 `.vscode/settings.json`:

```json
{
  "files.associations": {
    "*.mindsl": "yaml"
  }
}
```

---

## 기본 구조

```yaml
# 필수 필드
scale: <척도 ID>       # 영문 식별자 (예: PHQ-9, DASS-21)
version: "<버전>"      # 시맨틱 버전 (예: "1.0.0")

# 메타데이터 (선택)
meta:
  name:
    ko: "<한국어 이름>"
    en: "<English name>"
  category: <카테고리>   # depression, anxiety, personality, etc.
  time_frame: "<시간 프레임>"  # "지난 2주간", "평소에" 등
  citation: "<인용 정보>"

# 응답 유형 정의
response_type: <유형>
options: [...]

# 문항 정의
items: [...]

# 하위척도 (선택)
subscales: {...}

# 조건부 섹션 (선택)
sections: [...]

# 커스텀 함수 (선택)
functions: {...}

# 채점 로직
scoring: {...}
```

---

## 응답 유형 (Response Types)

### Likert 척도

```yaml
response_type: likert
options:
  - { value: 0, label: { ko: "전혀 아니다", en: "Not at all" } }
  - { value: 1, label: { ko: "며칠 동안", en: "Several days" } }
  - { value: 2, label: { ko: "1주일 이상", en: "More than half the days" } }
  - { value: 3, label: { ko: "거의 매일", en: "Nearly every day" } }
```

### 단축 문법

```yaml
# 숫자 범위 자동 생성
response_type: likert(0-3)

# 또는 likert(1-5), likert(1-7) 등
```

### 예/아니오

```yaml
response_type: binary
options:
  - { value: 0, label: { ko: "아니오", en: "No" } }
  - { value: 1, label: { ko: "예", en: "Yes" } }
```

### 숫자 입력

```yaml
response_type: number
unit: minutes  # 선택: minutes, hours, days, etc.
min: 0
max: 1440
```

### 시간 입력

```yaml
response_type: time
format: "HH:mm"  # 24시간 형식
```

### 텍스트 입력

```yaml
response_type: text
max_length: 500
```

---

## 문항 정의 (Items)

### 간단한 형식 (동일 응답 유형)

```yaml
items:
  - 일을 하는 것에 흥미나 재미가 거의 없음
  - 기분이 가라앉거나 우울하거나 희망이 없음
  - 잠들기 어렵거나 자주 깸, 또는 너무 많이 잠
```

자동으로 `q1`, `q2`, `q3` ID 할당

### 상세 형식

```yaml
items:
  - id: q1
    text:
      ko: "일을 하는 것에 흥미나 재미가 거의 없음"
      en: "Little interest or pleasure in doing things"

  - id: q2
    text:
      ko: "기분이 가라앉거나 우울하거나 희망이 없음"
      en: "Feeling down, depressed, or hopeless"
```

### 역채점 문항

```yaml
items:
  - id: q3
    text:
      ko: "나는 대체로 실패한 사람이라고 느끼는 경향이 있다"
    reverse: true  # 자동으로 역채점 처리
```

### 개별 응답 유형 지정

```yaml
items:
  - id: bedtime
    type: time
    text:
      ko: "평소 취침 시간"

  - id: sleep_latency
    type: number
    unit: minutes
    text:
      ko: "잠들기까지 걸린 시간(분)"
```

---

## 하위척도 (Subscales)

```yaml
subscales:
  depression: [q3, q5, q10, q13, q16, q17, q21]
  anxiety: [q2, q4, q7, q9, q15, q19, q20]
  stress: [q1, q6, q8, q11, q12, q14, q18]
```

채점에서 사용:

```yaml
scoring:
  depression_score: sum(depression) * 2
  anxiety_score: sum(anxiety) * 2
  stress_score: sum(stress) * 2
```

---

## 조건부 섹션 (Sections)

복잡한 분기 로직이 필요한 경우:

```yaml
sections:
  - id: screening
    items:
      - { id: q1, text: { ko: "기분이 들떠서 평소와 달랐다" } }
      - { id: q2, text: { ko: "잠을 적게 자도 괜찮았다" } }
      # ... q1-q13

  - id: followup
    condition: count(screening, == 1) >= 1  # 하나라도 "예"
    items:
      - id: q14
        text:
          ko: "위 증상들이 같은 시기에 나타났습니까?"
        type: binary

      - id: q15
        text:
          ko: "그로 인해 겪은 문제의 정도는?"
        type: likert(1-4)
```

---

## 표현식 언어

Python-like 문법을 사용합니다.

### 연산자

| 연산자 | 설명 | 예시 |
|--------|------|------|
| `+`, `-`, `*`, `/` | 산술 | `total / 9` |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | 비교 | `score >= 10` |
| `and`, `or`, `not` | 논리 | `q1 == 1 and q2 == 1` |
| `( )` | 그룹핑 | `(a + b) * c` |

### 조건문

```yaml
severity: |
  if total < 5:
    "minimal"
  elif total < 10:
    "mild"
  elif total < 15:
    "moderate"
  elif total < 20:
    "moderately_severe"
  else:
    "severe"
```

### 범위 참조

```yaml
# 연속 범위
sum(q1..q9)       # q1부터 q9까지
mean(q5a..q5j)    # q5a부터 q5j까지

# 전체 문항
sum(all)

# 하위척도
sum(depression)   # subscales에 정의된 그룹
```

---

## 채점 로직 (Scoring)

### 단순 합산

```yaml
scoring:
  total: sum(all)
```

### 구간 분류

```yaml
scoring:
  total: sum(all)
  severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])
```

`cut()` 함수:
- 첫 번째 인자: 점수
- 두 번째 인자: 경계값 배열 (오름차순)
- 세 번째 인자: 라벨 배열 (경계값보다 1개 많음)

### 복합 계산 (멀티라인)

```yaml
scoring:
  sleep_efficiency: |
    time_in_bed = hours_between(bedtime, waketime)
    efficiency = (sleep_hours / time_in_bed) * 100
    cut(efficiency, [85, 75, 65], [0, 1, 2, 3], reverse=true)
```

### 조건부 채점

```yaml
scoring:
  positive_screen: |
    symptom_count = count(q1..q13, == 1)
    same_time = q14 == 1
    impairment = q15 >= 2

    symptom_count >= 7 and same_time and impairment
```

---

## 커스텀 함수 (Functions)

튜링 완전한 로직이 필요한 경우:

```yaml
functions:
  weighted_mean: |
    fn(items, weights):
      total = 0
      weight_sum = 0
      for i, item in enumerate(items):
        total = total + (item * weights[i])
        weight_sum = weight_sum + weights[i]
      return total / weight_sum

  calculate_tscore: |
    fn(raw, mean, sd):
      return ((raw - mean) / sd) * 10 + 50

scoring:
  weighted: weighted_mean([q1, q2, q3], [1.5, 1.0, 2.0])
  tscore: calculate_tscore(total, 12.5, 4.2)
```

### 함수 문법

```
fn(<매개변수들>):
  <본문>
  return <결과>
```

### 지원 구문

- 변수 할당: `x = 10`
- 조건문: `if`, `elif`, `else`
- 반복문: `for item in items`, `for i in range(n)`
- 리스트: `[1, 2, 3]`
- 딕셔너리: `{key: value}`
- 인덱싱: `items[0]`, `items[i]`

---

## 다음 문서

- [빌트인 함수 목록](./BUILTINS.md)
- [예제 모음](./EXAMPLES.md)
