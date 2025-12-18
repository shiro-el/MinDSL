# CES-D (Center for Epidemiologic Studies Depression Scale)
# 역학연구 우울척도

## 1. 기본 정보

| 항목 | 내용 |
|------|------|
| **척도명** | CES-D (Center for Epidemiologic Studies Depression Scale) |
| **한국어명** | 역학연구 우울척도 |
| **카테고리** | 우울증 선별 |
| **문항 수** | 20문항 |
| **응답 시간** | 약 5-10분 |
| **대상** | 성인 |
| **평가 기간** | 지난 1주일 |

## 2. 원저자 및 출처

- **원저자**: Lenore S. Radloff
- **출판연도**: 1977
- **원문 출처**: Radloff, L. S. (1977). The CES-D scale: A self-report depression scale for research in the general population. *Applied Psychological Measurement, 1*(3), 385-401.
- **개발 기관**: National Institute of Mental Health (NIMH)

## 3. 저작권/라이선스

| 항목 | 내용 |
|------|------|
| **라이선스 유형** | 퍼블릭 도메인 (Public Domain) |
| **상업적 이용** | 가능 |
| **저작자 표시** | 권장 |

## 4. 한국어판 정보 및 타당화 연구

### 주요 타당화 연구
- **조맹제, 김계희 (1993)**: 한국판 CES-D 표준화 연구
- **전겸구 등 (2001)**: 통합적 CES-D 개발

### 심리측정적 속성
| 지표 | 일반 집단 | 임상 집단 |
|------|----------|----------|
| Cronbach's α | 0.91 | 0.93 |
| 민감도 | 93.5% | - |
| 특이도 | 79.2% | - |

### 절단점
- **지역사회 일반**: 16점
- **임상 집단**: 25점

## 5. 전체 문항

### 지시문
> 다음은 지난 일주일 동안 당신이 느끼고 행동한 것에 관한 내용입니다. 각 문항에 대해 지난 일주일 동안 얼마나 자주 그렇게 느꼈는지 표시해 주십시오.

### 문항 목록

| 번호 | 문항 | 역채점 |
|------|------|--------|
| 1 | 평소에는 아무렇지도 않던 일들이 귀찮게 느껴졌다 | |
| 2 | 먹고 싶지 않고 식욕이 없었다 | |
| 3 | 가족이나 친구가 도와주더라도 울적한 기분을 떨쳐버릴 수 없었다 | |
| 4 | **다른 사람들만큼 능력이 있다고 느꼈다** | ✓ |
| 5 | 무슨 일을 하든 정신을 집중하기가 힘들었다 | |
| 6 | 우울했다 | |
| 7 | 하는 일마다 힘들게 느껴졌다 | |
| 8 | **미래에 대해 희망적이라고 느꼈다** | ✓ |
| 9 | 내 인생은 실패작이라는 생각이 들었다 | |
| 10 | 두려움을 느꼈다 | |
| 11 | 잠을 설쳤다 (잠을 잘 이루지 못했다) | |
| 12 | **행복했다** | ✓ |
| 13 | 평소보다 말을 적게 했다 (말수가 줄었다) | |
| 14 | 세상에 홀로 있는 듯한 외로움을 느꼈다 | |
| 15 | 사람들이 나에게 차갑게 대하는 것 같았다 | |
| 16 | **생활이 즐거웠다** | ✓ |
| 17 | 갑자기 울음이 나왔다 | |
| 18 | 슬픔을 느꼈다 | |
| 19 | 사람들이 나를 싫어하는 것 같았다 | |
| 20 | 도무지 무엇을 시작할 기운이 나지 않았다 | |

> **역채점 문항**: 4, 8, 12, 16번 (긍정 정서 문항)

## 6. 응답 척도

| 점수 | 한국어 | 영어 | 빈도 |
|------|--------|------|------|
| 0 | 극히 드물게 | Rarely | 1일 미만 |
| 1 | 가끔 | Some | 1-2일 |
| 2 | 종종 | Occasionally | 3-4일 |
| 3 | 대부분 | Most | 5-7일 |

## 7. 채점 방식

### 역채점 문항
**문항 4, 8, 12, 16번**은 역채점:
```
역채점: 0→3, 1→2, 2→1, 3→0
```

### 채점 공식
```
총점 = (Q1 + Q2 + Q3 + Q5 + Q6 + Q7 + Q9 + Q10 + Q11 + Q13 + Q14 + Q15 + Q17 + Q18 + Q19 + Q20)
     + (3-Q4) + (3-Q8) + (3-Q12) + (3-Q16)
```

### 기본 정보
- **총점 범위**: 0-60점
- **역채점 문항**: 4, 8, 12, 16번

## 8. 해석 기준

| 총점 | 심각도 | 해석 |
|------|--------|------|
| 0-15 | 정상 | 우울 증상 없음 |
| 16-20 | 경증 우울 | 경미한 우울 증상 |
| 21-24 | 중등도 우울 | 중간 정도의 우울 증상 |
| 25-60 | 중증 우울 | 심각한 우울 증상, 전문 평가 필요 |

**선별 절단점**:
- 지역사회: 16점
- 임상적으로 유의한 우울: 25점

## 9. DSL 스키마 예시 (JSON)

```json
{
  "id": "ces-d",
  "metadata": {
    "name": "CES-D",
    "fullName": "Center for Epidemiologic Studies Depression Scale",
    "koreanName": "역학연구 우울척도",
    "category": "depression",
    "itemCount": 20,
    "license": "public-domain",
    "timeframe": "past_week"
  },
  "instructions": {
    "ko": "다음은 지난 일주일 동안 당신이 느끼고 행동한 것에 관한 내용입니다.",
    "en": "Below is a list of ways you might have felt or behaved during the past week."
  },
  "responseScale": {
    "type": "likert",
    "min": 0,
    "max": 3,
    "options": [
      { "value": 0, "ko": "극히 드물게 (1일 미만)", "en": "Rarely (less than 1 day)" },
      { "value": 1, "ko": "가끔 (1-2일)", "en": "Some (1-2 days)" },
      { "value": 2, "ko": "종종 (3-4일)", "en": "Occasionally (3-4 days)" },
      { "value": 3, "ko": "대부분 (5-7일)", "en": "Most (5-7 days)" }
    ]
  },
  "items": [
    { "id": 1, "ko": "평소에는 아무렇지도 않던 일들이 귀찮게 느껴졌다", "reverse": false },
    { "id": 2, "ko": "먹고 싶지 않고 식욕이 없었다", "reverse": false },
    { "id": 3, "ko": "울적한 기분을 떨쳐버릴 수 없었다", "reverse": false },
    { "id": 4, "ko": "다른 사람들만큼 능력이 있다고 느꼈다", "reverse": true },
    { "id": 5, "ko": "정신을 집중하기가 힘들었다", "reverse": false },
    { "id": 6, "ko": "우울했다", "reverse": false },
    { "id": 7, "ko": "하는 일마다 힘들게 느껴졌다", "reverse": false },
    { "id": 8, "ko": "미래에 대해 희망적이라고 느꼈다", "reverse": true },
    { "id": 9, "ko": "내 인생은 실패작이라는 생각이 들었다", "reverse": false },
    { "id": 10, "ko": "두려움을 느꼈다", "reverse": false },
    { "id": 11, "ko": "잠을 설쳤다", "reverse": false },
    { "id": 12, "ko": "행복했다", "reverse": true },
    { "id": 13, "ko": "평소보다 말을 적게 했다", "reverse": false },
    { "id": 14, "ko": "세상에 홀로 있는 듯한 외로움을 느꼈다", "reverse": false },
    { "id": 15, "ko": "사람들이 나에게 차갑게 대하는 것 같았다", "reverse": false },
    { "id": 16, "ko": "생활이 즐거웠다", "reverse": true },
    { "id": 17, "ko": "갑자기 울음이 나왔다", "reverse": false },
    { "id": 18, "ko": "슬픔을 느꼈다", "reverse": false },
    { "id": 19, "ko": "사람들이 나를 싫어하는 것 같았다", "reverse": false },
    { "id": 20, "ko": "도무지 무엇을 시작할 기운이 나지 않았다", "reverse": false }
  ],
  "scoring": {
    "method": "sum",
    "range": { "min": 0, "max": 60 },
    "reverseItems": [4, 8, 12, 16],
    "reverseFormula": "3 - rawScore",
    "interpretation": [
      { "range": [0, 15], "severity": "normal", "ko": "정상" },
      { "range": [16, 20], "severity": "mild", "ko": "경증 우울" },
      { "range": [21, 24], "severity": "moderate", "ko": "중등도 우울" },
      { "range": [25, 60], "severity": "severe", "ko": "중증 우울" }
    ],
    "cutoffs": { "community": 16, "clinical": 25 }
  }
}
```

## 참고문헌

1. Radloff, L. S. (1977). The CES-D scale: A self-report depression scale for research in the general population. *Applied Psychological Measurement, 1*(3), 385-401.
2. 조맹제, 김계희. (1993). 주요우울증환자 예비평가에서 CES-D의 진단적 타당성 연구. *신경정신의학, 32*(3), 381-399.
3. 전겸구, 최상진, 양병창. (2001). 통합적 한국판 CES-D 개발. *한국심리학회지: 건강, 6*(1), 59-76.
