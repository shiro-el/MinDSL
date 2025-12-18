# GAD-7 (Generalized Anxiety Disorder-7)
# 범불안장애 척도

## 1. 기본 정보

| 항목 | 내용 |
|------|------|
| **척도명** | GAD-7 (Generalized Anxiety Disorder-7) |
| **한국어명** | 범불안장애 척도 |
| **카테고리** | 불안 장애 선별 |
| **문항 수** | 7문항 |
| **응답 시간** | 약 2-5분 |
| **대상** | 성인 및 청소년 |
| **평가 기간** | 지난 2주일 |

## 2. 원저자 및 출처

- **원저자**: Robert L. Spitzer, Kurt Kroenke, Janet B.W. Williams, Bernd Löwe
- **출판연도**: 2006
- **원문 출처**: Spitzer, R. L., Kroenke, K., Williams, J. B., & Löwe, B. (2006). A brief measure for assessing generalized anxiety disorder: the GAD-7. *Archives of Internal Medicine, 166*(10), 1092-1097.

## 3. 저작권/라이선스

| 항목 | 내용 |
|------|------|
| **라이선스 유형** | 퍼블릭 도메인 (Public Domain) |
| **상업적 이용** | 가능 |
| **저작자 표시** | 권장 |
| **공식 사이트** | https://www.phqscreeners.com/ |

## 4. 한국어판 정보 및 타당화 연구

### 주요 타당화 연구
- **Seo & Park (2015)**: 편두통 환자 대상 타당화
- **Kim et al. (2019)**: 한국 대학생 대상 타당화

### 심리측정적 속성
| 지표 | 값 |
|------|-----|
| Cronbach's α | > 0.80 |
| 민감도 | 89% |
| 특이도 | 82% |

## 5. 전체 문항

### 지시문
> 지난 2주 동안, 다음의 문제들로 인해서 얼마나 자주 방해를 받았습니까?

### 문항 목록

| 번호 | 문항 (한국어) | 문항 (영어) |
|------|--------------|-------------|
| 1 | 초조하거나 불안하거나 조마조마하게 느낀다 | Feeling nervous, anxious, or on edge |
| 2 | 걱정하는 것을 멈추거나 조절할 수 없다 | Not being able to stop or control worrying |
| 3 | 여러 가지 것들에 대해 걱정을 너무 많이 한다 | Worrying too much about different things |
| 4 | 편하게 있기가 어렵다 | Trouble relaxing |
| 5 | 너무 불안해서 가만히 있을 수 없다 | Being so restless that it's hard to sit still |
| 6 | 쉽게 짜증이 나거나 쉽게 성을 낸다 | Becoming easily annoyed or irritable |
| 7 | 마치 뭔가 끔찍한 일이 일어날 것처럼 두렵게 느낀다 | Feeling afraid as if something awful might happen |

## 6. 응답 척도

| 점수 | 한국어 | 영어 |
|------|--------|------|
| 0 | 전혀 그렇지 않다 | Not at all |
| 1 | 며칠 동안 | Several days |
| 2 | 절반 이상의 날 동안 | More than half the days |
| 3 | 거의 매일 | Nearly every day |

## 7. 채점 방식

### 기본 채점
- **방법**: 7개 문항 점수 단순 합산
- **총점 범위**: 0-21점
- **역채점 문항**: 없음

### 채점 공식
```
총점 = Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7
```

## 8. 해석 기준

| 총점 | 심각도 | 권장 사항 |
|------|--------|-----------|
| 0-4 | 정상 (Minimal) | 임상적 불안 없음 |
| 5-9 | 경미한 불안 (Mild) | 추적 관찰 |
| 10-14 | 중등도 불안 (Moderate) | 추가 평가 권장 |
| 15-21 | 심한 불안 (Severe) | 전문가 상담 필요 |

**선별 절단점**: 10점 이상

## 9. DSL 스키마 예시 (JSON)

```json
{
  "id": "gad-7",
  "metadata": {
    "name": "GAD-7",
    "fullName": "Generalized Anxiety Disorder-7",
    "koreanName": "범불안장애 척도",
    "category": "anxiety",
    "itemCount": 7,
    "license": "public-domain",
    "timeframe": "past_2_weeks"
  },
  "instructions": {
    "ko": "지난 2주 동안, 다음의 문제들로 인해서 얼마나 자주 방해를 받았습니까?",
    "en": "Over the last 2 weeks, how often have you been bothered by the following problems?"
  },
  "responseScale": {
    "type": "likert",
    "min": 0,
    "max": 3,
    "options": [
      { "value": 0, "ko": "전혀 그렇지 않다", "en": "Not at all" },
      { "value": 1, "ko": "며칠 동안", "en": "Several days" },
      { "value": 2, "ko": "절반 이상의 날 동안", "en": "More than half the days" },
      { "value": 3, "ko": "거의 매일", "en": "Nearly every day" }
    ]
  },
  "items": [
    { "id": 1, "ko": "초조하거나 불안하거나 조마조마하게 느낀다", "reverse": false },
    { "id": 2, "ko": "걱정하는 것을 멈추거나 조절할 수 없다", "reverse": false },
    { "id": 3, "ko": "여러 가지 것들에 대해 걱정을 너무 많이 한다", "reverse": false },
    { "id": 4, "ko": "편하게 있기가 어렵다", "reverse": false },
    { "id": 5, "ko": "너무 불안해서 가만히 있을 수 없다", "reverse": false },
    { "id": 6, "ko": "쉽게 짜증이 나거나 쉽게 성을 낸다", "reverse": false },
    { "id": 7, "ko": "마치 뭔가 끔찍한 일이 일어날 것처럼 두렵게 느낀다", "reverse": false }
  ],
  "scoring": {
    "method": "sum",
    "range": { "min": 0, "max": 21 },
    "reverseItems": [],
    "interpretation": [
      { "range": [0, 4], "severity": "minimal", "ko": "정상" },
      { "range": [5, 9], "severity": "mild", "ko": "경미한 불안" },
      { "range": [10, 14], "severity": "moderate", "ko": "중등도 불안" },
      { "range": [15, 21], "severity": "severe", "ko": "심한 불안" }
    ],
    "cutoffs": { "screening": 10 }
  }
}
```

## 참고문헌

1. Spitzer, R. L., Kroenke, K., Williams, J. B., & Löwe, B. (2006). A brief measure for assessing generalized anxiety disorder: the GAD-7. *Archives of Internal Medicine, 166*(10), 1092-1097.
2. Seo, J. G., & Park, S. P. (2015). Validation of the Generalized Anxiety Disorder-7 (GAD-7) and GAD-2 in patients with migraine. *The Journal of Headache and Pain, 16*(1), 97.
