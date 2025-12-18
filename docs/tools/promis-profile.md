# PROMIS Profile (Patient-Reported Outcomes Measurement Information System)

## 1. 기본 정보

**정식 명칭**: Patient-Reported Outcomes Measurement Information System (PROMIS) Profile
**목적**: 환자보고 건강상태 측정 - 신체적, 정신적, 사회적 건강 영역 평가
**소요 시간**: 5-10분 (프로필 버전에 따라 다름)
**대상**: 만 18세 이상 성인 (아동용 별도 버전 존재)
**문항 수**: 29문항 또는 43문항 또는 57문항 (프로필 버전별 상이)
**점수 방식**: T-score (평균 50, 표준편차 10)

## 2. 원저자 및 출처

**개발 기관**:
- National Institutes of Health (NIH)
- Northwestern University (NU)
- 미국 국립보건원 로드맵 이니셔티브 (NIH Roadmap Initiative)

**주요 개발자**:
- David Cella, PhD (Principal Investigator, Northwestern University)
- Richard Gershon, PhD
- Ron Hays, PhD
- PROMIS Cooperative Group (다기관 협력)

**개발 기간**: 2004년 시작, 지속적 업데이트
**공식 웹사이트**: https://www.healthmeasures.net

**주요 논문**:
- Cella, D., et al. (2010). "The Patient-Reported Outcomes Measurement Information System (PROMIS): Progress of an NIH Roadmap Cooperative Group During its First Two Years." *Medical Care*, 48(6 Suppl), S1-S10.
- Cella, D., et al. (2007). "The Patient-Reported Outcomes Measurement Information System (PROMIS) Developed and Tested its First Wave of Adult Self-Reported Health Outcome Item Banks: 2005-2008." *Journal of Clinical Epidemiology*, 63(11), 1179-1194.

## 3. 저작권/라이선스

**저작권 소유**: U.S. Department of Health and Human Services
**라이선스**: **무료 오픈 액세스**

### 사용 조건
- **임상/연구/교육 목적**: 완전 무료
- **상업적 사용**: 무료 사용 가능
- **전자화/디지털 구현**: 무료 (HealthMeasures API 제공)
- **번역**: 공식 번역 프로토콜 준수 시 무료
- **수정**: 문항 수정 불가, 선택적 사용 가능

### 사용 요구사항
- 출처 명시: "PROMIS® [instrument name] was developed with funding from the National Institutes of Health (NIH)"
- 공식 버전 사용 (www.healthmeasures.net에서 다운로드)
- 점수 계산은 공식 스코어링 매뉴얼 준수

### 공식 리소스
- 다운로드: https://www.healthmeasures.net/explore-measurement-systems/promis
- API: https://www.assessmentcenter.net
- 스코어링 서비스: HealthMeasures Scoring Service (무료)

## 4. 한국어판 정보

**한국어 번역**: 공식 번역 진행 중 (일부 영역 완료)

**번역 가용성**:
- **완료된 영역**:
  - Physical Function (신체기능)
  - Pain Interference (통증 간섭)
  - Fatigue (피로)
  - Depression (우울)
  - Anxiety (불안)
  - Sleep Disturbance (수면 장애)
  - Social Participation (사회적 참여)

**번역 검증**:
- 공식 PROMIS 번역 프로토콜 준수
- 순번역-역번역 검증
- 인지적 면담 (cognitive debriefing)
- 문화적 타당성 평가

**한국어판 접근**:
- 공식 사이트에서 다운로드: https://www.healthmeasures.net
- 언어 선택: Korean (한국어)

**타당도 연구**:
- 한국 만성 질환자 대상 타당도 연구 진행 중
- 일부 영역은 신뢰도 및 타당도 확인 완료

## 5. 평가 영역 및 구조 설명

PROMIS Profile은 **모듈식 구조**로 설계되어 있으며, 연구자/임상가가 필요에 따라 영역을 선택할 수 있습니다.

### 5.1 PROMIS-29 Profile v2.1 (가장 많이 사용됨)

29개 문항 + 1개 통증 강도 평가 = 총 30개 문항

#### 영역별 구성

**1. Physical Function (신체기능)** - 4문항
- **측정 내용**: 일상생활 신체활동 수행 능력
- **문항 예시**:
  - "계단을 한 층 올라갈 수 있습니까?"
  - "무거운 물건을 들거나 나를 수 있습니까?"
  - "집안일을 할 수 있습니까?"
- **응답 척도**: 5점 (전혀 못함 - 어려움 없이 할 수 있음)

**2. Anxiety (불안)** - 4문항
- **측정 내용**: 불안 증상의 빈도와 심각도
- **문항 예시**:
  - "나는 긴장감을 느꼈다"
  - "나는 불안감에 압도되었다"
  - "나는 걱정이 많았다"
- **응답 척도**: 5점 (전혀 없음 - 항상)
- **시간틀**: 지난 7일간

**3. Depression (우울)** - 4문항
- **측정 내용**: 우울 증상의 빈도와 심각도
- **문항 예시**:
  - "나는 슬픔을 느꼈다"
  - "나는 무가치하다고 느꼈다"
  - "나는 아무것도 기대할 것이 없다고 느꼈다"
- **응답 척도**: 5점 (전혀 없음 - 항상)
- **시간틀**: 지난 7일간

**4. Fatigue (피로)** - 4문항
- **측정 내용**: 피로의 영향과 심각도
- **문항 예시**:
  - "나는 피곤함을 느꼈다"
  - "나는 일상적인 활동을 하기에 너무 피곤했다"
  - "나는 정신적으로 지쳐 있었다"
- **응답 척도**: 5점 (전혀 없음 - 매우 심함)
- **시간틀**: 지난 7일간

**5. Sleep Disturbance (수면 장애)** - 4문항
- **측정 내용**: 수면의 질과 수면 문제
- **문항 예시**:
  - "내 수면의 질은 어떠했습니까?"
  - "나는 잠들기 어려웠다"
  - "나는 수면에 문제가 있었다"
- **응답 척도**: 5점 (매우 좋음/전혀 없음 - 매우 나쁨/매우 심함)
- **시간틀**: 지난 7일간

**6. Ability to Participate in Social Roles and Activities (사회적 역할 및 활동 참여 능력)** - 4문항
- **측정 내용**: 사회적 역할 수행 능력
- **문항 예시**:
  - "나는 가족 활동에 참여할 수 있었다"
  - "나는 친구들과 함께하는 활동을 할 수 있었다"
  - "나는 일(직장 포함)을 할 수 있었다"
- **응답 척도**: 5점 (전혀 못함 - 항상 할 수 있음)
- **시간틀**: 지난 7일간

**7. Pain Interference (통증 간섭)** - 4문항
- **측정 내용**: 통증이 일상생활에 미치는 영향
- **문항 예시**:
  - "통증이 가족과 함께하는 활동을 얼마나 방해했습니까?"
  - "통증이 집안일을 얼마나 방해했습니까?"
  - "통증이 즐거움을 느끼는 것을 얼마나 방해했습니까?"
- **응답 척도**: 5점 (전혀 없음 - 매우 심함)
- **시간틀**: 지난 7일간

**8. Pain Intensity (통증 강도)** - 1문항
- **측정 내용**: 평균 통증 수준
- **문항**: "지난 7일간 평균 통증 수준을 평가해주세요"
- **응답 척도**: 0-10 숫자 평가 척도 (NRS)
  - 0 = 통증 없음
  - 10 = 상상할 수 있는 가장 심한 통증

### 5.2 PROMIS-43 Profile v2.0

43개 문항으로 각 영역당 6-8문항 포함 (더 정밀한 평가)

**추가/확장 영역**:
- Cognitive Function (인지기능) - 8문항
- Physical Function - 6문항 (확장)
- 기타 영역도 문항 수 증가

### 5.3 PROMIS-57 Profile v2.0

57개 문항으로 가장 포괄적인 평가

**추가 영역**:
- Cognitive Function - Abilities (인지기능 - 능력)
- Emotional Distress - Anger (감정적 고통 - 분노)

### 5.4 응답 형식

**5점 리커트 척도** (영역별로 척도 레이블 상이)

**신체기능**:
- 5 = 어려움 없이 할 수 있음 (Without any difficulty)
- 4 = 약간의 어려움 (With a little difficulty)
- 3 = 어느 정도 어려움 (With some difficulty)
- 2 = 많은 어려움 (With much difficulty)
- 1 = 전혀 못함 (Unable to do)

**증상 빈도** (불안, 우울, 피로):
- 1 = 전혀 없음 (Never)
- 2 = 드물게 (Rarely)
- 3 = 가끔 (Sometimes)
- 4 = 자주 (Often)
- 5 = 항상 (Always)

**심각도** (통증 간섭):
- 1 = 전혀 없음 (Not at all)
- 2 = 약간 (A little bit)
- 3 = 어느 정도 (Somewhat)
- 4 = 꽤 많이 (Quite a bit)
- 5 = 매우 심함 (Very much)

## 6. 채점 방식

PROMIS는 **Item Response Theory (IRT)** 기반 채점을 사용합니다.

### 6.1 T-score 체계

**기준**:
- **평균**: 50 (미국 일반 인구 기준)
- **표준편차**: 10
- **범위**: 일반적으로 10-90 (이론적으로 무한대)

**해석**:
- T-score 50 = 평균 수준 (일반 인구)
- 높은 점수 = 더 많은 특성/증상
  - 증상 영역 (예: 불안, 우울): 높을수록 나쁨
  - 기능 영역 (예: 신체기능, 사회참여): 높을수록 좋음

### 6.2 채점 방법

**방법 1: 공식 스코어링 서비스 (권장)**
- HealthMeasures Scoring Service 사용
- URL: https://www.assessmentcenter.net/ac_scoringservice
- 무료, 자동 계산
- IRT 기반 정확한 T-score 제공

**방법 2: 스코어링 매뉴얼 사용**
- 각 영역별 Raw Score → T-score 변환표 제공
- 단계:
  1. Raw score 계산 (문항 점수 합계)
  2. 변환표에서 해당 T-score 찾기

**방법 3: API 통합**
- REDCap 또는 HealthMeasures API 사용
- 자동 채점 및 데이터 관리

### 6.3 채점 예시 (PROMIS-29)

**예시: Anxiety 영역**

| 문항 | 응답 | 점수 |
|-----|------|-----|
| 1. 긴장감 | 자주 | 4 |
| 2. 불안감 압도 | 가끔 | 3 |
| 3. 걱정 | 자주 | 4 |
| 4. 공포 | 가끔 | 3 |
| **Raw Score** | | **14** |

**변환**:
- Raw Score 14 → T-score 약 62
- 해석: 평균보다 1.2 표준편차 높음 (경도-중등도 불안)

### 6.4 Missing Data 처리

- **원칙**: 각 영역당 최소 50% 문항 응답 필요
- PROMIS-29의 경우:
  - 4문항 영역: 최소 2문항 응답 필요
  - 응답 없는 문항이 50% 초과 시 해당 영역 점수 산출 불가

## 7. 해석 기준

### 7.1 T-score 해석 가이드라인

#### 일반 해석 (표준편차 기준)

| T-score 범위 | 해석 | 표준편차 |
|-------------|------|---------|
| < 40 | 평균 이하 (기능: 우수, 증상: 경미) | -1 SD 이하 |
| 40-45 | 평균보다 약간 낮음 | -1 to -0.5 SD |
| 45-55 | 평균 범위 (정상) | ±0.5 SD |
| 55-60 | 평균보다 약간 높음 | +0.5 to +1 SD |
| 60-70 | 경도-중등도 (관심 필요) | +1 to +2 SD |
| > 70 | 중증 (개입 필요) | +2 SD 이상 |

### 7.2 영역별 임상적 절단점

#### Physical Function (신체기능)
- **높음 = 좋음**
- T ≥ 50: 정상 범위
- T 40-49: 경도 제한
- T 30-39: 중등도 제한
- T < 30: 중증 장애

#### Anxiety (불안)
- **높음 = 나쁨**
- T < 55: 정상 범위
- T 55-59.9: 경도 불안
- T 60-69.9: 중등도 불안
- T ≥ 70: 중증 불안

#### Depression (우울)
- **높음 = 나쁨**
- T < 55: 정상 범위
- T 55-59.9: 경도 우울
- T 60-69.9: 중등도 우울
- T ≥ 70: 중증 우울 (자살 위험 평가 필요)

#### Fatigue (피로)
- **높음 = 나쁨**
- T < 55: 정상 범위
- T 55-59.9: 경도 피로
- T 60-69.9: 중등도 피로
- T ≥ 70: 중증 피로

#### Sleep Disturbance (수면 장애)
- **높음 = 나쁨**
- T < 55: 정상 수면
- T 55-59.9: 경도 수면 장애
- T 60-69.9: 중등도 수면 장애
- T ≥ 70: 중증 수면 장애

#### Social Participation (사회적 참여)
- **높음 = 좋음**
- T ≥ 50: 정상 참여
- T 40-49: 경도 제한
- T 30-39: 중등도 제한
- T < 30: 중증 제한

#### Pain Interference (통증 간섭)
- **높음 = 나쁨**
- T < 55: 정상 범위 (최소 간섭)
- T 55-59.9: 경도 간섭
- T 60-69.9: 중등도 간섭
- T ≥ 70: 중증 간섭

#### Pain Intensity (통증 강도)
- **0-10 척도 직접 해석**
- 0: 통증 없음
- 1-3: 경도 통증
- 4-6: 중등도 통증
- 7-10: 중증 통증

### 7.3 Minimally Important Difference (MID)

**임상적으로 유의미한 변화**:
- **일반 원칙**: T-score 변화 4-5점
- 영역별 MID:
  - Physical Function: 4-5점
  - Pain Interference: 3-5점
  - Fatigue: 3-5점
  - Depression: 5-8점
  - Anxiety: 4-6점

### 7.4 프로필 패턴 해석

**예시 1: 만성통증 환자**
- Pain Interference: T=65 (중등도-중증)
- Pain Intensity: 7/10 (중증)
- Physical Function: T=35 (중등도 제한)
- Depression: T=58 (경도)
- Social Participation: T=40 (경도 제한)
→ 통증이 기능과 사회생활에 광범위한 영향

**예시 2: 우울/불안 환자**
- Depression: T=68 (중등도-중증)
- Anxiety: T=64 (중등도)
- Sleep Disturbance: T=62 (중등도)
- Fatigue: T=66 (중등도-중증)
- Social Participation: T=38 (중등도 제한)
→ 정서 증상이 수면, 에너지, 사회기능에 영향

### 7.5 종단 평가 (추적 관찰)

**변화 해석**:
- **개선**: T-score 감소 (증상 영역) 또는 증가 (기능 영역)
- **악화**: T-score 증가 (증상 영역) 또는 감소 (기능 영역)
- **유의미한 변화**: MID 이상 (약 4-5점)

**예시**:
- 치료 전 Depression T=65
- 치료 후 Depression T=58
- 변화: -7점 → 임상적으로 유의미한 개선

### 7.6 다른 도구와의 비교

| PROMIS 영역 | 유사 도구 | 상관계수 |
|-----------|---------|---------|
| Depression | PHQ-9 | r = 0.78-0.85 |
| Anxiety | GAD-7 | r = 0.75-0.82 |
| Pain Interference | BPI Interference | r = 0.80-0.88 |
| Physical Function | SF-36 PF | r = 0.75-0.85 |
| Fatigue | FACIT-Fatigue | r = 0.80-0.90 |

## 8. DSL 스키마 예시 (JSON)

```json
{
  "assessment": {
    "id": "promis-29-profile-v2.1",
    "name": "PROMIS-29 Profile v2.1",
    "name_ko": "PROMIS-29 프로필 v2.1",
    "version": "2.1",
    "language": "en",
    "languages_available": ["en", "es", "ko", "zh", "de", "fr"],
    "developer": "National Institutes of Health (NIH)",
    "principal_investigator": "David Cella, PhD",
    "license": "free_open_access",
    "license_url": "https://www.healthmeasures.net",
    "copyright": "U.S. Department of Health and Human Services",
    "administration_time": 10,
    "total_items": 30,
    "target_population": {
      "age_min": 18,
      "age_max": null,
      "conditions": ["chronic_disease", "cancer", "general_health"]
    },
    "scoring": {
      "method": "IRT_based",
      "score_type": "T-score",
      "mean": 50,
      "standard_deviation": 10,
      "range": [10, 90],
      "reference_population": "US_general_population_2000_census",
      "scoring_service": "https://www.assessmentcenter.net/ac_scoringservice"
    },
    "domains": [
      {
        "id": "physical_function",
        "name": "Physical Function",
        "name_ko": "신체기능",
        "code": "PFA",
        "items": 4,
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Unable to do",
            "2": "With much difficulty",
            "3": "With some difficulty",
            "4": "With a little difficulty",
            "5": "Without any difficulty"
          }
        },
        "direction": "higher_better",
        "interpretation": {
          "normal": [50, null],
          "mild_impairment": [40, 49],
          "moderate_impairment": [30, 39],
          "severe_impairment": [null, 29]
        },
        "mid": 4.5
      },
      {
        "id": "anxiety",
        "name": "Anxiety",
        "name_ko": "불안",
        "code": "EDANX",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Never",
            "2": "Rarely",
            "3": "Sometimes",
            "4": "Often",
            "5": "Always"
          }
        },
        "direction": "higher_worse",
        "interpretation": {
          "normal": [null, 54.9],
          "mild": [55, 59.9],
          "moderate": [60, 69.9],
          "severe": [70, null]
        },
        "mid": 5.0
      },
      {
        "id": "depression",
        "name": "Depression",
        "name_ko": "우울",
        "code": "EDDEP",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Never",
            "2": "Rarely",
            "3": "Sometimes",
            "4": "Often",
            "5": "Always"
          }
        },
        "direction": "higher_worse",
        "interpretation": {
          "normal": [null, 54.9],
          "mild": [55, 59.9],
          "moderate": [60, 69.9],
          "severe": [70, null]
        },
        "mid": 6.5,
        "clinical_note": "T>=70: Assess suicide risk"
      },
      {
        "id": "fatigue",
        "name": "Fatigue",
        "name_ko": "피로",
        "code": "FATIGUE",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Not at all",
            "2": "A little bit",
            "3": "Somewhat",
            "4": "Quite a bit",
            "5": "Very much"
          }
        },
        "direction": "higher_worse",
        "interpretation": {
          "normal": [null, 54.9],
          "mild": [55, 59.9],
          "moderate": [60, 69.9],
          "severe": [70, null]
        },
        "mid": 4.0
      },
      {
        "id": "sleep_disturbance",
        "name": "Sleep Disturbance",
        "name_ko": "수면 장애",
        "code": "SLEEP29",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Very good / Not at all",
            "2": "Good / A little bit",
            "3": "Fair / Somewhat",
            "4": "Poor / Quite a bit",
            "5": "Very poor / Very much"
          }
        },
        "direction": "higher_worse",
        "interpretation": {
          "normal": [null, 54.9],
          "mild": [55, 59.9],
          "moderate": [60, 69.9],
          "severe": [70, null]
        },
        "mid": 4.5
      },
      {
        "id": "social_participation",
        "name": "Ability to Participate in Social Roles and Activities",
        "name_ko": "사회적 역할 및 활동 참여 능력",
        "code": "SRPSAT",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Never",
            "2": "Rarely",
            "3": "Sometimes",
            "4": "Usually",
            "5": "Always"
          }
        },
        "direction": "higher_better",
        "interpretation": {
          "normal": [50, null],
          "mild_impairment": [40, 49],
          "moderate_impairment": [30, 39],
          "severe_impairment": [null, 29]
        },
        "mid": 4.0
      },
      {
        "id": "pain_interference",
        "name": "Pain Interference",
        "name_ko": "통증 간섭",
        "code": "PAININ",
        "items": 4,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "likert_5",
          "min": 1,
          "max": 5,
          "labels": {
            "1": "Not at all",
            "2": "A little bit",
            "3": "Somewhat",
            "4": "Quite a bit",
            "5": "Very much"
          }
        },
        "direction": "higher_worse",
        "interpretation": {
          "normal": [null, 54.9],
          "mild": [55, 59.9],
          "moderate": [60, 69.9],
          "severe": [70, null]
        },
        "mid": 4.0
      },
      {
        "id": "pain_intensity",
        "name": "Pain Intensity",
        "name_ko": "통증 강도",
        "code": "PAININT",
        "items": 1,
        "timeframe": "past_7_days",
        "response_scale": {
          "type": "numeric_rating_scale",
          "min": 0,
          "max": 10,
          "labels": {
            "0": "No pain",
            "10": "Worst imaginable pain"
          }
        },
        "score_type": "raw_score",
        "interpretation": {
          "none": [0, 0],
          "mild": [1, 3],
          "moderate": [4, 6],
          "severe": [7, 10]
        }
      }
    ],
    "profile_patterns": [
      {
        "type": "chronic_pain",
        "name": "만성통증 패턴",
        "characteristics": {
          "pain_interference": [60, null],
          "pain_intensity": [7, null],
          "physical_function": [null, 40],
          "social_participation": [null, 45]
        }
      },
      {
        "type": "depression_anxiety",
        "name": "우울/불안 패턴",
        "characteristics": {
          "depression": [60, null],
          "anxiety": [60, null],
          "sleep_disturbance": [60, null],
          "fatigue": [60, null]
        }
      },
      {
        "type": "cancer_treatment",
        "name": "암 치료 부작용 패턴",
        "characteristics": {
          "fatigue": [65, null],
          "physical_function": [null, 40],
          "social_participation": [null, 45],
          "depression": [55, null]
        }
      }
    ],
    "missing_data": {
      "rule": "50_percent_rule",
      "description": "각 영역당 최소 50% 문항 응답 필요",
      "minimum_items_per_domain": 2
    },
    "validation": {
      "reliability": {
        "cronbach_alpha": [0.89, 0.98],
        "test_retest": [0.82, 0.93]
      },
      "validity": {
        "convergent": {
          "PHQ-9": 0.82,
          "GAD-7": 0.79,
          "SF-36": 0.80
        },
        "known_groups": "Excellent discrimination"
      },
      "responsiveness": {
        "effect_size": [0.45, 0.85],
        "mid": [3, 6.5]
      }
    },
    "clinical_use": {
      "applications": [
        "임상 환자 모니터링",
        "치료 효과 평가",
        "연구 결과 측정",
        "인구 건강 조사",
        "레지스트리 데이터 수집"
      ],
      "advantages": [
        "무료 사용 가능",
        "정밀한 측정 (IRT 기반)",
        "다양한 언어 버전",
        "표준화된 T-score",
        "짧은 소요 시간",
        "모듈식 구조 (필요에 따라 선택)"
      ],
      "considerations": [
        "T-score 해석에 대한 교육 필요",
        "미국 인구 기반 규준 (문화적 차이 고려)",
        "컴퓨터 기반 시행 권장 (자동 채점)",
        "심각한 증상 시 추가 평가 필요"
      ]
    }
  }
}
```

## 참고문헌

1. Cella, D., et al. (2010). The Patient-Reported Outcomes Measurement Information System (PROMIS): Progress of an NIH Roadmap Cooperative Group During its First Two Years. *Medical Care*, 48(6 Suppl), S1-S10.

2. Cella, D., et al. (2019). PROMIS® Adult Health Profiles: Efficient Short-Form Measures of Seven Health Domains. *Value in Health*, 22(5), 537-544.

3. Hays, R. D., et al. (2018). PROMIS®-29 v2.0 profile physical and mental health summary scores. *Quality of Life Research*, 27(7), 1885-1891.

4. HealthMeasures. PROMIS Scoring Manuals. https://www.healthmeasures.net/score-and-interpret/calculate-scores

5. Pilkonis, P. A., et al. (2011). Item Banks for Measuring Emotional Distress From the Patient-Reported Outcomes Measurement Information System (PROMIS®): Depression, Anxiety, and Anger. *Assessment*, 18(3), 263-283.

---

**추가 리소스**:
- 공식 웹사이트: https://www.healthmeasures.net
- 스코어링 서비스: https://www.assessmentcenter.net/ac_scoringservice
- API 문서: https://www.assessmentcenter.net/documents/
- 번역 및 문화적응: https://www.healthmeasures.net/explore-measurement-systems/promis/intro-to-promis/available-translations

**주의사항**: PROMIS는 선별 및 모니터링 도구이며, 임상 진단을 대체하지 않습니다. 중증 증상 (예: T≥70) 발견 시 추가 평가 및 전문가 상담이 필요합니다.
