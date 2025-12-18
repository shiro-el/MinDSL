# Rosenberg Self-Esteem Scale (RSES)
# 로젠버그 자존감 척도

## 1. 기본 정보

| 항목 | 내용 |
|------|------|
| **척도명** | Rosenberg Self-Esteem Scale (RSES) |
| **한국어명** | 로젠버그 자존감 척도 |
| **카테고리** | 자존감 평가 |
| **문항 수** | 10문항 |
| **응답 시간** | 약 2-3분 |
| **대상** | 청소년 및 성인 |

## 2. 원저자 및 출처

- **원저자**: Morris Rosenberg
- **출판연도**: 1965
- **원문 출처**: Rosenberg, M. (1965). *Society and the adolescent self-image*. Princeton, NJ: Princeton University Press.

## 3. 저작권/라이선스

| 항목 | 내용 |
|------|------|
| **라이선스 유형** | 퍼블릭 도메인 (Public Domain) |
| **상업적 이용** | 가능 |
| **저작자 표시** | 권장 |

## 4. 한국어판 정보 및 타당화 연구

### 주요 타당화 연구
- **전병재 (1974)**: 최초 한국어 번안
- **이훈진, 원호택 (1995)**: 타당화 연구

### 심리측정적 속성
| 지표 | 값 |
|------|-----|
| Cronbach's α | 0.76-0.88 |
| 검사-재검사 | 높은 안정성 |

## 5. 전체 문항

### 문항 목록

| 번호 | 문항 | 역채점 |
|------|------|--------|
| 1 | 나는 내가 다른 사람들처럼 가치 있는 사람이라고 생각한다 | |
| 2 | 나는 좋은 성품을 가졌다고 생각한다 | |
| 3 | **나는 대체적으로 실패한 사람이라는 느낌이 든다** | ✓ |
| 4 | 나는 대부분의 다른 사람들과 같이 일을 잘 할 수가 있다 | |
| 5 | **나는 자랑할 것이 별로 없다** | ✓ |
| 6 | 나는 내 자신에 대하여 긍정적인 태도를 가지고 있다 | |
| 7 | 나는 내 자신에 대하여 대체로 만족한다 | |
| 8 | **나는 내 자신을 좀 더 존경할 수 있으면 좋겠다** | ✓ |
| 9 | **나는 가끔 내 자신이 쓸모없는 사람이라는 느낌이 든다** | ✓ |
| 10 | **나는 때때로 내가 좋지 않은 사람이라고 생각한다** | ✓ |

> **역채점 문항**: 3, 5, 8, 9, 10번

## 6. 응답 척도

| 점수 | 한국어 | 영어 |
|------|--------|------|
| 1 | 전혀 그렇지 않다 | Strongly Disagree |
| 2 | 그렇지 않다 | Disagree |
| 3 | 그렇다 | Agree |
| 4 | 매우 그렇다 | Strongly Agree |

## 7. 채점 방식

### 역채점 문항
**문항 3, 5, 8, 9, 10번** 역채점:
```
역채점: 1→4, 2→3, 3→2, 4→1
```

### 채점 공식
```
총점 = (Q1 + Q2 + Q4 + Q6 + Q7) + [(5-Q3) + (5-Q5) + (5-Q8) + (5-Q9) + (5-Q10)]
```

### 기본 정보
- **총점 범위**: 10-40점
- **고득점**: 높은 자존감
- **저득점**: 낮은 자존감

## 8. 해석 기준

| 총점 | 심각도 | 해석 |
|------|--------|------|
| 10-20 | 매우 낮음 | 임상적 주의 필요 |
| 21-25 | 낮음 | 자존감 낮은 편 |
| 26-30 | 보통 | 평균 수준 |
| 31-35 | 높음 | 건강한 자존감 |
| 36-40 | 매우 높음 | 매우 긍정적 자기 인식 |

**임상적 기준**: 20점 이하 시 전문가 상담 권장

## 9. DSL 스키마 예시 (JSON)

```json
{
  "id": "rosenberg-ses",
  "metadata": {
    "name": "Rosenberg SES",
    "fullName": "Rosenberg Self-Esteem Scale",
    "koreanName": "로젠버그 자존감 척도",
    "category": "self-esteem",
    "itemCount": 10,
    "license": "public-domain"
  },
  "responseScale": {
    "type": "likert",
    "min": 1,
    "max": 4,
    "options": [
      { "value": 1, "ko": "전혀 그렇지 않다", "en": "Strongly Disagree" },
      { "value": 2, "ko": "그렇지 않다", "en": "Disagree" },
      { "value": 3, "ko": "그렇다", "en": "Agree" },
      { "value": 4, "ko": "매우 그렇다", "en": "Strongly Agree" }
    ]
  },
  "items": [
    { "id": 1, "ko": "나는 내가 다른 사람들처럼 가치 있는 사람이라고 생각한다", "reverse": false },
    { "id": 2, "ko": "나는 좋은 성품을 가졌다고 생각한다", "reverse": false },
    { "id": 3, "ko": "나는 대체적으로 실패한 사람이라는 느낌이 든다", "reverse": true },
    { "id": 4, "ko": "나는 대부분의 다른 사람들과 같이 일을 잘 할 수가 있다", "reverse": false },
    { "id": 5, "ko": "나는 자랑할 것이 별로 없다", "reverse": true },
    { "id": 6, "ko": "나는 내 자신에 대하여 긍정적인 태도를 가지고 있다", "reverse": false },
    { "id": 7, "ko": "나는 내 자신에 대하여 대체로 만족한다", "reverse": false },
    { "id": 8, "ko": "나는 내 자신을 좀 더 존경할 수 있으면 좋겠다", "reverse": true },
    { "id": 9, "ko": "나는 가끔 내 자신이 쓸모없는 사람이라는 느낌이 든다", "reverse": true },
    { "id": 10, "ko": "나는 때때로 내가 좋지 않은 사람이라고 생각한다", "reverse": true }
  ],
  "scoring": {
    "method": "sum",
    "range": { "min": 10, "max": 40 },
    "reverseItems": [3, 5, 8, 9, 10],
    "reverseFormula": "5 - rawScore",
    "interpretation": [
      { "range": [10, 20], "severity": "very_low", "ko": "매우 낮음" },
      { "range": [21, 25], "severity": "low", "ko": "낮음" },
      { "range": [26, 30], "severity": "normal", "ko": "보통" },
      { "range": [31, 35], "severity": "high", "ko": "높음" },
      { "range": [36, 40], "severity": "very_high", "ko": "매우 높음" }
    ]
  }
}
```

## 참고문헌

1. Rosenberg, M. (1965). *Society and the adolescent self-image*. Princeton, NJ: Princeton University Press.
2. 전병재 (1974). 자아개념 측정가능성에 관한 연구. *연세논총, 11*, 107-130.
3. 이훈진, 원호택 (1995). 자기개념과 편집증적 경향. *심리과학, 4*(2), 15-29.
