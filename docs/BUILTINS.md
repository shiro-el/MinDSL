# MinDSL 빌트인 함수

## 집계 함수 (Aggregation)

### `sum()`

문항들의 합계를 계산합니다.

```yaml
sum(all)              # 모든 문항 합계
sum(q1..q9)           # q1부터 q9까지 합계
sum(depression)       # 하위척도 합계
sum(q1, q3, q5)       # 특정 문항들 합계
```

역채점 문항(`reverse: true`)은 자동으로 역변환 후 합산됩니다.

---

### `mean()`

문항들의 평균을 계산합니다.

```yaml
mean(all)             # 전체 평균
mean(q1..q5)          # 범위 평균
mean(subscale_a)      # 하위척도 평균
```

---

### `count()`

조건을 만족하는 문항의 개수를 셉니다.

```yaml
count(all, >= 2)         # 2 이상인 문항 개수
count(q1..q13, == 1)     # 1인 문항 개수 (예: "예" 응답 개수)
count(screening, != 0)   # 0이 아닌 문항 개수
```

---

### `min()` / `max()`

최소/최대값을 반환합니다.

```yaml
min(all)              # 전체 중 최소값
max(q1..q9)           # 범위 중 최대값
```

---

### `std()` / `var()`

표준편차/분산을 계산합니다.

```yaml
std(all)              # 표준편차
var(subscale_a)       # 분산
```

---

## 변환 함수 (Transformation)

### `cut()`

연속 점수를 구간(카테고리)으로 변환합니다.

```yaml
cut(score, [경계값들], [라벨들])
cut(score, [경계값들], [라벨들], reverse=true)
```

**예시:**

```yaml
# PHQ-9 심각도 분류
severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])

# 해석:
# total < 5  → "minimal"
# 5 <= total < 10 → "mild"
# 10 <= total < 15 → "moderate"
# 15 <= total < 20 → "moderately_severe"
# total >= 20 → "severe"
```

**역방향 (높을수록 좋은 경우):**

```yaml
# 수면 효율: 높을수록 좋음
efficiency_score: cut(efficiency, [85, 75, 65], [0, 1, 2, 3], reverse=true)

# 해석:
# efficiency >= 85 → 0
# 75 <= efficiency < 85 → 1
# 65 <= efficiency < 75 → 2
# efficiency < 65 → 3
```

---

### `reverse()`

점수를 역변환합니다.

```yaml
reverse(q3)                    # 단일 문항 역변환
reverse(q3, min=0, max=3)      # 범위 명시적 지정
```

보통은 문항 정의에서 `reverse: true`로 처리하지만, 채점 로직에서 명시적으로 사용할 수도 있습니다.

---

### `scale()`

점수를 다른 범위로 변환합니다.

```yaml
scale(score, from=[0, 27], to=[0, 100])  # 0-27 → 0-100으로 변환
```

---

### `round()`

반올림합니다.

```yaml
round(mean(all))              # 정수로 반올림
round(mean(all), 2)           # 소수점 2자리
```

---

### `floor()` / `ceil()`

내림/올림합니다.

```yaml
floor(score / 3)
ceil(total / items_count)
```

---

## 논리 함수 (Logic)

### `any()`

하나라도 조건을 만족하면 `true`

```yaml
any(q1..q9, >= 3)            # 하나라도 3 이상이면 true
any(screening, == 1)          # 하나라도 "예"이면 true
```

---

### `all()`

모두 조건을 만족하면 `true`

```yaml
all(q1..q5, != 0)            # 모두 0이 아니면 true
all(required_items, is_answered)  # 모두 응답했으면 true
```

---

### `none()`

모두 조건을 만족하지 않으면 `true`

```yaml
none(q1..q5, == 4)           # 4인 응답이 하나도 없으면 true
```

---

## 시간 함수 (Time)

### `hours_between()`

두 시간 사이의 시간(hours)을 계산합니다.

```yaml
hours_between(bedtime, waketime)  # 예: 23:00 ~ 07:00 → 8
```

자정을 넘어가는 경우 자동 처리됩니다.

---

### `minutes_between()`

두 시간 사이의 분(minutes)을 계산합니다.

```yaml
minutes_between(start_time, end_time)
```

---

## 통계 함수 (Statistics)

### `percentile()`

백분위수를 계산합니다.

```yaml
percentile(score, norms)      # norms 기준 백분위
```

---

### `zscore()`

Z 점수를 계산합니다.

```yaml
zscore(raw, mean, sd)         # (raw - mean) / sd
```

---

### `tscore()`

T 점수를 계산합니다 (평균 50, 표준편차 10).

```yaml
tscore(raw, mean, sd)         # ((raw - mean) / sd) * 10 + 50
```

---

## 유틸리티 함수 (Utility)

### `coalesce()`

첫 번째로 null이 아닌 값을 반환합니다.

```yaml
coalesce(q1, q1_alternative, 0)  # q1이 없으면 대체값, 그것도 없으면 0
```

---

### `is_answered()`

문항이 응답되었는지 확인합니다.

```yaml
is_answered(q5)               # q5가 응답되었으면 true
count(all, is_answered)       # 응답된 문항 수
```

---

### `missing_count()`

응답되지 않은 문항 수를 반환합니다.

```yaml
missing_count(all)
missing_count(q1..q9)
```

---

### `range()`

반복문에서 숫자 범위를 생성합니다.

```yaml
for i in range(10):       # 0부터 9까지
for i in range(1, 11):    # 1부터 10까지
```

---

### `enumerate()`

인덱스와 값을 함께 반복합니다.

```yaml
for i, item in enumerate(items):
  # i: 0, 1, 2, ...
  # item: 각 문항의 값
```

---

### `len()`

리스트의 길이를 반환합니다.

```yaml
len(items)
len(depression)  # 하위척도 문항 수
```

---

## 확장 예정

다음 함수들은 향후 추가 예정입니다:

| 함수 | 설명 | 용도 |
|------|------|------|
| `irt_theta()` | IRT 능력 추정 | 적응형 검사 |
| `irt_info()` | 문항 정보 함수 | 다음 문항 선택 |
| `cronbach_alpha()` | 내적 일관성 | 신뢰도 계산 |
| `factor_score()` | 요인 점수 | 요인 분석 결과 적용 |
