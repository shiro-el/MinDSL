# PHQ-9 (Patient Health Questionnaire-9)
# 환자 건강 질문지-9

## 1. 기본 정보

| 항목 | 내용 |
|------|------|
| **척도명** | PHQ-9 (Patient Health Questionnaire-9) |
| **한국어명** | 환자 건강 질문지-9 |
| **카테고리** | 우울증 선별 |
| **문항 수** | 9문항 |
| **응답 시간** | 약 2-3분 |
| **대상** | 청소년 및 성인 |
| **평가 기간** | 지난 2주일 |

## 2. 원저자 및 출처

- **원저자**: Kurt Kroenke, Robert L. Spitzer, Janet B.W. Williams
- **출판연도**: 2001
- **원문 출처**: Kroenke, K., Spitzer, R. L., & Williams, J. B. (2001). The PHQ-9: validity of a brief depression severity measure. *Journal of General Internal Medicine, 16*(9), 606-613.
- **DOI**: 10.1046/j.1525-1497.2001.016009606.x

## 3. 저작권/라이선스

| 항목 | 내용 |
|------|------|
| **라이선스 유형** | 퍼블릭 도메인 (Public Domain) |
| **상업적 이용** | 가능 |
| **저작자 표시** | 권장 |
| **수정 가능** | 가능 (원본 형태 사용 권장) |

## 4. 한국어판 정보 및 타당화 연구

### 주요 타당화 연구
- **박승진 등 (2010)**: 한글판 PHQ-9 신뢰도와 타당도
- **안제용 등 (2013)**: 한국어판 PHQ-9 표준화 연구

### 심리측정적 속성
| 지표 | 값 |
|------|-----|
| Cronbach's α | 0.81-0.95 |
| 검사-재검사 신뢰도 | r = 0.89 |
| 민감도 (절단점 5점) | 89.9% |
| 특이도 (절단점 5점) | 84.1% |

### 절단점
- **일반 성인**: 5점
- **주요우울증**: 9점

## 5. 전체 문항

### 지시문
> 지난 2주일 동안, 당신은 다음의 문제들로 인해서 얼마나 자주 방해를 받았습니까?

### 문항 목록

| 번호 | 문항 (한국어) | 문항 (영어) |
|------|--------------|-------------|
| 1 | 일 또는 여가 활동을 하는 데 흥미나 즐거움을 느끼지 못함 | Little interest or pleasure in doing things |
| 2 | 기분이 가라앉거나, 우울하거나, 희망이 없다고 느낌 | Feeling down, depressed, or hopeless |
| 3 | 잠이 들거나 잠을 자는 것이 어려움, 또는 잠을 너무 많이 잠 | Trouble falling or staying asleep, or sleeping too much |
| 4 | 피곤하다고 느끼거나 기운이 거의 없음 | Feeling tired or having little energy |
| 5 | 입맛이 없거나 과식을 함 | Poor appetite or overeating |
| 6 | 자신을 부정적으로 봄 - 혹은 자신이 실패자라고 느끼거나 자신 또는 가족을 실망시킴 | Feeling bad about yourself |
| 7 | 신문을 읽거나 텔레비전 보는 것과 같은 일에 집중하는 것이 어려움 | Trouble concentrating on things |
| 8 | 다른 사람들이 주목할 정도로 너무 느리게 움직이거나 말을 함. 또는 반대로 평상시보다 많이 움직여서, 너무 안절부절못하거나 들떠 있음 | Moving or speaking slowly/restlessly |
| 9 | 자신이 죽는 것이 더 낫다고 생각하거나 어떤 식으로든 자신을 해칠 것이라고 생각함 ⚠️ | Thoughts of self-harm or suicide |

> ⚠️ **문항 9 주의**: 자살사고 문항으로, 0점 초과 시 즉각적인 임상 평가 필요

## 6. 응답 척도

| 점수 | 한국어 | 영어 |
|------|--------|------|
| 0 | 전혀 방해 받지 않았다 | Not at all |
| 1 | 며칠 동안 | Several days |
| 2 | 7일 이상 | More than half the days |
| 3 | 거의 매일 | Nearly every day |

## 7. 채점 방식

### 기본 채점
- **방법**: 9개 문항 점수 단순 합산
- **총점 범위**: 0-27점
- **역채점 문항**: 없음

### 채점 공식
```
총점 = Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9
```

## 8. 해석 기준

| 총점 | 심각도 | 권장 사항 |
|------|--------|-----------|
| 0-4 | 우울 상태 아님 (Minimal) | 특별한 조치 불필요 |
| 5-9 | 경증 우울 (Mild) | 추적 관찰 권장 |
| 10-14 | 중등도 우울 (Moderate) | 치료 계획 수립 고려 |
| 15-19 | 중등도-중증 우울 (Moderately Severe) | 적극적 치료 권장 |
| 20-27 | 중증 우울 (Severe) | 즉각적 치료 개입 필요 |

## 9. DSL 스키마 예시 (JSON)

```json
{
  "id": "phq-9",
  "metadata": {
    "name": "PHQ-9",
    "fullName": "Patient Health Questionnaire-9",
    "koreanName": "환자 건강 질문지-9",
    "category": "depression",
    "itemCount": 9,
    "license": "public-domain",
    "timeframe": "past_2_weeks"
  },
  "instructions": {
    "ko": "지난 2주일 동안, 당신은 다음의 문제들로 인해서 얼마나 자주 방해를 받았습니까?",
    "en": "Over the last 2 weeks, how often have you been bothered by any of the following problems?"
  },
  "responseScale": {
    "type": "likert",
    "min": 0,
    "max": 3,
    "options": [
      { "value": 0, "ko": "전혀 방해 받지 않았다", "en": "Not at all" },
      { "value": 1, "ko": "며칠 동안", "en": "Several days" },
      { "value": 2, "ko": "7일 이상", "en": "More than half the days" },
      { "value": 3, "ko": "거의 매일", "en": "Nearly every day" }
    ]
  },
  "items": [
    { "id": 1, "ko": "일 또는 여가 활동을 하는 데 흥미나 즐거움을 느끼지 못함", "reverse": false },
    { "id": 2, "ko": "기분이 가라앉거나, 우울하거나, 희망이 없다고 느낌", "reverse": false },
    { "id": 3, "ko": "잠이 들거나 잠을 자는 것이 어려움, 또는 잠을 너무 많이 잠", "reverse": false },
    { "id": 4, "ko": "피곤하다고 느끼거나 기운이 거의 없음", "reverse": false },
    { "id": 5, "ko": "입맛이 없거나 과식을 함", "reverse": false },
    { "id": 6, "ko": "자신을 부정적으로 봄", "reverse": false },
    { "id": 7, "ko": "집중하는 것이 어려움", "reverse": false },
    { "id": 8, "ko": "너무 느리게 또는 안절부절못하게 움직임", "reverse": false },
    { "id": 9, "ko": "자해/자살 생각", "reverse": false, "riskFlag": true }
  ],
  "scoring": {
    "method": "sum",
    "range": { "min": 0, "max": 27 },
    "reverseItems": [],
    "interpretation": [
      { "range": [0, 4], "severity": "minimal", "ko": "우울 상태 아님" },
      { "range": [5, 9], "severity": "mild", "ko": "경증 우울" },
      { "range": [10, 14], "severity": "moderate", "ko": "중등도 우울" },
      { "range": [15, 19], "severity": "moderately_severe", "ko": "중등도-중증 우울" },
      { "range": [20, 27], "severity": "severe", "ko": "중증 우울" }
    ],
    "cutoffs": {
      "screening": 5,
      "majorDepression": 9,
      "treatmentRecommended": 10
    }
  },
  "validation": {
    "cronbachAlpha": 0.81,
    "sensitivity": 0.899,
    "specificity": 0.841
  }
}
```

## 참고문헌

1. Kroenke, K., Spitzer, R. L., & Williams, J. B. (2001). The PHQ-9: validity of a brief depression severity measure. *Journal of General Internal Medicine, 16*(9), 606-613.
2. 박승진, 최혜라, 최지혜, 김건우, 홍진표. (2010). 한글판 우울증 선별도구(PHQ-9)의 신뢰도와 타당도. *대한불안의학회지, 6*(2), 119-124.
3. 안제용, 서은란, 임경희, 신재현, 김정범. (2013). 한국어판 PHQ-9의 표준화 연구. *생물정신의학, 19*(1), 47-56.
