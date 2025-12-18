# MinDSL 예제 모음

실제 심리 평가 도구를 MinDSL로 구현한 예제들입니다.

---

## 레벨 1: 단순 합산

### PHQ-9 (환자 건강 질문지-9)

가장 간단한 형태의 척도입니다.

```yaml
# scales/depression/phq9.mindsl

scale: PHQ-9
version: "1.0.0"

meta:
  name:
    ko: "환자 건강 질문지-9"
    en: "Patient Health Questionnaire-9"
  category: depression
  time_frame:
    ko: "지난 2주간"
    en: "Over the last 2 weeks"
  citation: "Kroenke et al. (2001)"

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
  - id: q3
    text:
      ko: "잠들기 어렵거나 자주 깸, 또는 너무 많이 잠"
      en: "Trouble falling or staying asleep, or sleeping too much"
  - id: q4
    text:
      ko: "피곤함, 기력이 없음"
      en: "Feeling tired or having little energy"
  - id: q5
    text:
      ko: "식욕 감소 또는 과식"
      en: "Poor appetite or overeating"
  - id: q6
    text:
      ko: "자신에 대한 부정적 생각"
      en: "Feeling bad about yourself"
  - id: q7
    text:
      ko: "집중하기 어려움"
      en: "Trouble concentrating on things"
  - id: q8
    text:
      ko: "느리게 움직이거나 반대로 안절부절못함"
      en: "Moving or speaking slowly, or being fidgety"
  - id: q9
    text:
      ko: "자해 또는 자살에 대한 생각"
      en: "Thoughts of self-harm or suicide"

scoring:
  total: sum(all)
  severity: cut(total, [5, 10, 15, 20], [minimal, mild, moderate, moderately_severe, severe])
```

---

### GAD-7 (범불안장애척도-7)

PHQ-9과 유사한 구조입니다.

```yaml
# scales/anxiety/gad7.mindsl

scale: GAD-7
version: "1.0.0"

meta:
  name:
    ko: "범불안장애척도-7"
    en: "Generalized Anxiety Disorder 7-item"
  category: anxiety
  time_frame:
    ko: "지난 2주간"
    en: "Over the last 2 weeks"
  citation: "Spitzer et al. (2006)"

response_type: likert
options:
  - { value: 0, label: { ko: "전혀 아니다", en: "Not at all" } }
  - { value: 1, label: { ko: "며칠 동안", en: "Several days" } }
  - { value: 2, label: { ko: "1주일 이상", en: "More than half the days" } }
  - { value: 3, label: { ko: "거의 매일", en: "Nearly every day" } }

items:
  - id: q1
    text:
      ko: "초조하거나 불안하거나 조마조마하게 느낌"
      en: "Feeling nervous, anxious, or on edge"
  - id: q2
    text:
      ko: "걱정을 멈추거나 조절할 수 없음"
      en: "Not being able to stop or control worrying"
  - id: q3
    text:
      ko: "여러 가지 것들에 대해 지나치게 걱정함"
      en: "Worrying too much about different things"
  - id: q4
    text:
      ko: "편하게 쉬기 어려움"
      en: "Trouble relaxing"
  - id: q5
    text:
      ko: "너무 안절부절못해서 가만히 있기 어려움"
      en: "Being so restless that it is hard to sit still"
  - id: q6
    text:
      ko: "쉽게 짜증이 나거나 화가 남"
      en: "Becoming easily annoyed or irritable"
  - id: q7
    text:
      ko: "무서운 일이 일어날 것 같은 두려움"
      en: "Feeling afraid, as if something awful might happen"

scoring:
  total: sum(all)
  severity: cut(total, [5, 10, 15], [minimal, mild, moderate, severe])
```

---

## 레벨 2: 역채점 + 하위척도

### Rosenberg Self-Esteem Scale (로젠버그 자존감 척도)

역채점 문항이 포함된 척도입니다.

```yaml
# scales/self-esteem/rosenberg.mindsl

scale: Rosenberg-SES
version: "1.0.0"

meta:
  name:
    ko: "로젠버그 자존감 척도"
    en: "Rosenberg Self-Esteem Scale"
  category: self_esteem
  citation: "Rosenberg (1965)"

response_type: likert
options:
  - { value: 1, label: { ko: "전혀 그렇지 않다", en: "Strongly disagree" } }
  - { value: 2, label: { ko: "그렇지 않다", en: "Disagree" } }
  - { value: 3, label: { ko: "그렇다", en: "Agree" } }
  - { value: 4, label: { ko: "매우 그렇다", en: "Strongly agree" } }

items:
  - id: q1
    text:
      ko: "나는 내가 적어도 다른 사람만큼 가치 있는 사람이라고 느낀다"
      en: "I feel that I am a person of worth, at least on an equal plane with others"
  - id: q2
    text:
      ko: "나는 좋은 자질을 많이 가지고 있다고 느낀다"
      en: "I feel that I have a number of good qualities"
  - id: q3
    text:
      ko: "나는 대체로 실패한 사람이라고 느끼는 경향이 있다"
      en: "All in all, I am inclined to feel that I am a failure"
    reverse: true
  - id: q4
    text:
      ko: "나는 대부분의 다른 사람들만큼 일을 잘 할 수 있다"
      en: "I am able to do things as well as most other people"
  - id: q5
    text:
      ko: "나는 자랑할 것이 별로 없다고 느낀다"
      en: "I feel I do not have much to be proud of"
    reverse: true
  - id: q6
    text:
      ko: "나는 나 자신에 대해 긍정적인 태도를 가지고 있다"
      en: "I take a positive attitude toward myself"
  - id: q7
    text:
      ko: "전반적으로 나는 나 자신에 만족한다"
      en: "On the whole, I am satisfied with myself"
  - id: q8
    text:
      ko: "나는 나 자신을 좀 더 존경할 수 있으면 좋겠다"
      en: "I wish I could have more respect for myself"
    reverse: true
  - id: q9
    text:
      ko: "나는 때때로 내가 쓸모없다고 느낀다"
      en: "I certainly feel useless at times"
    reverse: true
  - id: q10
    text:
      ko: "나는 때때로 내가 좋지 않은 사람이라고 생각한다"
      en: "At times I think I am no good at all"
    reverse: true

scoring:
  total: sum(all)  # reverse 자동 처리
  # 10-40점 범위, 높을수록 높은 자존감
```

---

### DASS-21 (우울 불안 스트레스 척도-21)

하위척도가 있는 척도입니다.

```yaml
# scales/mental-health/dass21.mindsl

scale: DASS-21
version: "1.0.0"

meta:
  name:
    ko: "우울 불안 스트레스 척도-21"
    en: "Depression Anxiety Stress Scales-21"
  category: mental_health
  time_frame:
    ko: "지난 1주일간"
    en: "Over the past week"
  citation: "Lovibond & Lovibond (1995)"

response_type: likert
options:
  - { value: 0, label: { ko: "전혀 해당되지 않음", en: "Did not apply to me at all" } }
  - { value: 1, label: { ko: "약간 해당됨", en: "Applied to me to some degree" } }
  - { value: 2, label: { ko: "상당히 해당됨", en: "Applied to me a considerable degree" } }
  - { value: 3, label: { ko: "매우 많이 해당됨", en: "Applied to me very much" } }

items:
  - { id: q1, text: { ko: "사소한 일에 화가 났다" } }
  - { id: q2, text: { ko: "입이 마르는 것을 느꼈다" } }
  - { id: q3, text: { ko: "긍정적인 감정을 느끼지 못했다" } }
  - { id: q4, text: { ko: "호흡이 곤란했다" } }
  - { id: q5, text: { ko: "무엇을 시작하기 어려웠다" } }
  - { id: q6, text: { ko: "상황에 과민하게 반응했다" } }
  - { id: q7, text: { ko: "몸이 떨렸다" } }
  - { id: q8, text: { ko: "신경을 많이 소모했다" } }
  - { id: q9, text: { ko: "나를 당황하게 하는 상황이 걱정되었다" } }
  - { id: q10, text: { ko: "기대할 것이 없다고 느꼈다" } }
  - { id: q11, text: { ko: "동요하게 되는 것을 느꼈다" } }
  - { id: q12, text: { ko: "긴장을 풀기 어려웠다" } }
  - { id: q13, text: { ko: "슬프고 우울했다" } }
  - { id: q14, text: { ko: "방해받으면 참을 수 없었다" } }
  - { id: q15, text: { ko: "공황 상태가 될 것 같았다" } }
  - { id: q16, text: { ko: "어떤 것에도 열정을 느끼지 못했다" } }
  - { id: q17, text: { ko: "인간으로서 가치가 없다고 느꼈다" } }
  - { id: q18, text: { ko: "예민하다고 느꼈다" } }
  - { id: q19, text: { ko: "심장 박동을 느꼈다" } }
  - { id: q20, text: { ko: "이유 없이 두려웠다" } }
  - { id: q21, text: { ko: "삶이 의미 없다고 느꼈다" } }

subscales:
  depression: [q3, q5, q10, q13, q16, q17, q21]
  anxiety: [q2, q4, q7, q9, q15, q19, q20]
  stress: [q1, q6, q8, q11, q12, q14, q18]

scoring:
  depression_raw: sum(depression)
  anxiety_raw: sum(anxiety)
  stress_raw: sum(stress)

  # DASS-42 등가 점수 (× 2)
  depression: depression_raw * 2
  anxiety: anxiety_raw * 2
  stress: stress_raw * 2

  depression_severity: cut(depression, [10, 14, 21, 28], [normal, mild, moderate, severe, extremely_severe])
  anxiety_severity: cut(anxiety, [8, 10, 15, 20], [normal, mild, moderate, severe, extremely_severe])
  stress_severity: cut(stress, [15, 19, 26, 34], [normal, mild, moderate, severe, extremely_severe])
```

---

## 레벨 3: 복합 알고리즘

### PSQI (피츠버그 수면 질 지수)

복잡한 계산 로직이 필요한 척도입니다.

```yaml
# scales/sleep/psqi.mindsl

scale: PSQI
version: "1.0.0"

meta:
  name:
    ko: "피츠버그 수면 질 지수"
    en: "Pittsburgh Sleep Quality Index"
  category: sleep
  time_frame:
    ko: "지난 1개월간"
    en: "During the past month"
  citation: "Buysse et al. (1989)"

items:
  # 시간 입력 문항
  - id: bedtime
    type: time
    text:
      ko: "지난 한 달간, 보통 밤에 몇 시에 잠자리에 들었습니까?"
      en: "During the past month, what time have you usually gone to bed at night?"

  - id: sleep_latency_mins
    type: number
    unit: minutes
    text:
      ko: "지난 한 달간, 잠자리에 든 후 잠들기까지 보통 몇 분 걸렸습니까?"
      en: "How long (in minutes) has it usually taken you to fall asleep each night?"

  - id: waketime
    type: time
    text:
      ko: "지난 한 달간, 보통 아침에 몇 시에 일어났습니까?"
      en: "During the past month, what time have you usually gotten up in the morning?"

  - id: sleep_hours
    type: number
    unit: hours
    text:
      ko: "지난 한 달간, 밤에 실제로 몇 시간 잤습니까?"
      en: "During the past month, how many hours of actual sleep did you get at night?"

  # 수면 방해 요인 (q5a-q5j)
  - id: q5a
    text:
      ko: "30분 내에 잠들 수 없었다"
    response_type: likert
    options:
      - { value: 0, label: { ko: "지난 달에 없었음" } }
      - { value: 1, label: { ko: "주 1회 미만" } }
      - { value: 2, label: { ko: "주 1-2회" } }
      - { value: 3, label: { ko: "주 3회 이상" } }

  - { id: q5b, text: { ko: "한밤중이나 새벽에 깼다" } }
  - { id: q5c, text: { ko: "화장실에 가려고 일어나야 했다" } }
  - { id: q5d, text: { ko: "편하게 숨 쉴 수 없었다" } }
  - { id: q5e, text: { ko: "기침하거나 시끄럽게 코를 골았다" } }
  - { id: q5f, text: { ko: "너무 춥다고 느꼈다" } }
  - { id: q5g, text: { ko: "너무 덥다고 느꼈다" } }
  - { id: q5h, text: { ko: "나쁜 꿈을 꾸었다" } }
  - { id: q5i, text: { ko: "통증이 있었다" } }
  - { id: q5j, text: { ko: "기타 (구체적으로)" } }

  # 수면제 사용
  - id: q6
    text:
      ko: "지난 한 달간, 처방약 또는 비처방약을 얼마나 자주 복용했습니까?"

  # 주간 기능장애
  - id: q7
    text:
      ko: "지난 한 달간, 운전, 식사, 사회활동 중 깨어 있기 힘들었던 적이 얼마나 됩니까?"

  - id: q8
    text:
      ko: "지난 한 달간, 일을 열정적으로 하는 것이 얼마나 문제가 되었습니까?"

  # 주관적 수면의 질
  - id: q9
    text:
      ko: "지난 한 달간, 전반적으로 본인의 수면의 질을 어떻게 평가하시겠습니까?"
    response_type: likert
    options:
      - { value: 0, label: { ko: "매우 좋음" } }
      - { value: 1, label: { ko: "꽤 좋음" } }
      - { value: 2, label: { ko: "꽤 나쁨" } }
      - { value: 3, label: { ko: "매우 나쁨" } }

scoring:
  # Component 1: 주관적 수면의 질
  c1: q9

  # Component 2: 수면 잠복기
  c2: |
    latency_score = cut(sleep_latency_mins, [15, 30, 60], [0, 1, 2, 3])
    combined = latency_score + q5a
    cut(combined, [1, 3, 5], [0, 1, 2, 3])

  # Component 3: 수면 시간
  c3: |
    if sleep_hours > 7:
      0
    elif sleep_hours >= 6:
      1
    elif sleep_hours >= 5:
      2
    else:
      3

  # Component 4: 습관적 수면 효율
  c4: |
    time_in_bed = hours_between(bedtime, waketime)
    efficiency = (sleep_hours / time_in_bed) * 100
    if efficiency >= 85:
      0
    elif efficiency >= 75:
      1
    elif efficiency >= 65:
      2
    else:
      3

  # Component 5: 수면 방해
  c5: |
    disturbance_sum = sum(q5b..q5j)
    cut(disturbance_sum, [1, 10, 19], [0, 1, 2, 3])

  # Component 6: 수면제 사용
  c6: q6

  # Component 7: 주간 기능장애
  c7: |
    daytime_sum = q7 + q8
    cut(daytime_sum, [1, 3, 5], [0, 1, 2, 3])

  # Global PSQI Score
  global: sum(c1, c2, c3, c4, c5, c6, c7)

  # 해석: 5 이상이면 "poor sleeper"
  quality: |
    if global > 5:
      "poor"
    else:
      "good"
```

---

## 레벨 4: 조건 분기

### MDQ (기분장애 질문지)

조건부 섹션이 있는 척도입니다.

```yaml
# scales/bipolar/mdq.mindsl

scale: MDQ
version: "1.0.0"

meta:
  name:
    ko: "기분장애 질문지"
    en: "Mood Disorder Questionnaire"
  category: bipolar
  citation: "Hirschfeld et al. (2000)"

sections:
  - id: screening
    instruction:
      ko: "평소와 달리 기분이 너무 좋거나 들떠 있어서 다음과 같은 경험을 한 적이 있습니까?"
      en: "Has there ever been a period of time when you were not your usual self and..."

    response_type: binary
    options:
      - { value: 0, label: { ko: "아니오", en: "No" } }
      - { value: 1, label: { ko: "예", en: "Yes" } }

    items:
      - { id: q1, text: { ko: "기분이 너무 좋거나 들떠서 다른 사람들이 평소와 다르다고 했다" } }
      - { id: q2, text: { ko: "너무 짜증이 나서 소리를 지르거나 싸움을 하거나 했다" } }
      - { id: q3, text: { ko: "평소보다 훨씬 자신감이 넘쳤다" } }
      - { id: q4, text: { ko: "평소보다 훨씬 잠을 덜 자거나, 잠잘 필요를 느끼지 못했다" } }
      - { id: q5, text: { ko: "평소보다 말이 훨씬 많거나 매우 빨리 말했다" } }
      - { id: q6, text: { ko: "생각이 빠르게 지나갔다" } }
      - { id: q7, text: { ko: "주변 일에 쉽게 산만해져 집중하기 어려웠다" } }
      - { id: q8, text: { ko: "평소보다 훨씬 에너지가 넘쳤다" } }
      - { id: q9, text: { ko: "평소보다 훨씬 활동적이거나, 더 많은 일을 했다" } }
      - { id: q10, text: { ko: "평소보다 훨씬 사교적이었다" } }
      - { id: q11, text: { ko: "평소보다 성관계에 관심이 훨씬 많았다" } }
      - { id: q12, text: { ko: "평소와 다르게 행동하거나 다른 사람들이 이상하다고 생각할 행동을 했다" } }
      - { id: q13, text: { ko: "돈 쓰는 것으로 자신이나 가족에게 문제가 생겼다" } }

  - id: followup
    condition: count(screening, == 1) >= 1  # 하나라도 "예"면 표시

    items:
      - id: q14
        text:
          ko: "위에서 '예'라고 답한 것들 중 여러 가지가 같은 시기에 일어난 적이 있습니까?"
          en: "If you checked YES to more than one of the above, have several of these ever happened during the same period of time?"
        response_type: binary
        options:
          - { value: 0, label: { ko: "아니오", en: "No" } }
          - { value: 1, label: { ko: "예", en: "Yes" } }

      - id: q15
        text:
          ko: "이러한 것들로 인해 얼마나 문제가 생겼습니까?"
          en: "How much of a problem did any of these cause you?"
        response_type: likert
        options:
          - { value: 1, label: { ko: "문제가 없었음", en: "No problem" } }
          - { value: 2, label: { ko: "약간 문제가 됨", en: "Minor problem" } }
          - { value: 3, label: { ko: "중간 정도 문제", en: "Moderate problem" } }
          - { value: 4, label: { ko: "심각한 문제", en: "Serious problem" } }

scoring:
  symptom_count: count(screening, == 1)
  same_time: q14 == 1
  impairment: q15 >= 3  # 중간 정도 이상

  positive_screen: |
    symptom_count >= 7 and same_time and impairment
```

---

## 레벨 5: 커스텀 함수

### 가중치 평균 예제

```yaml
# scales/custom/weighted-example.mindsl

scale: Weighted-Example
version: "1.0.0"

items:
  - { id: q1, text: { ko: "문항 1" } }
  - { id: q2, text: { ko: "문항 2" } }
  - { id: q3, text: { ko: "문항 3" } }

response_type: likert(0-4)

functions:
  weighted_mean: |
    fn(items, weights):
      total = 0
      weight_sum = 0
      for i, item in enumerate(items):
        total = total + (item * weights[i])
        weight_sum = weight_sum + weights[i]
      return total / weight_sum

scoring:
  # q1은 가중치 1.5, q2는 1.0, q3은 2.0
  weighted_score: weighted_mean([q1, q2, q3], [1.5, 1.0, 2.0])

  # 일반 평균과 비교
  simple_mean: mean(all)
```

---

## 실제 사용 예시

### CSV 데이터에 있는 50개 척도 구현 순서 제안

1. **간단한 것부터**: PHQ-9, GAD-7, K-6, K-10
2. **역채점 추가**: Rosenberg SES, BIS-11
3. **하위척도**: DASS-21, PANAS
4. **복합 알고리즘**: PSQI, AUDIT
5. **조건 분기**: MDQ, ASRS
