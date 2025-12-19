"use client"

import { useState } from "react"
import type { Scale, Responses, ScoringResults } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import { ScaleHeader } from "./scale-header"
import { ScaleItem } from "./scale-item"
import { ScaleResults } from "./scale-results"
import { ProgressBar } from "./progress-bar"
import { Button } from "@/components/ui/button"
import { Card, CardContent } from "@/components/ui/card"
import {
  calculateScores,
  validateResponses,
  applyReverseScoring,
} from "@/lib/scoring-engine"

interface ScaleViewProps {
  scale: Scale
}

export function ScaleView({ scale }: ScaleViewProps) {
  const { language } = useLanguage()

  const [responses, setResponses] = useState<Responses>(() => {
    const initial: Responses = {}
    for (const item of scale.items) {
      initial[item.id] = null
    }
    return initial
  })
  const [errors, setErrors] = useState<Set<string>>(new Set())
  const [results, setResults] = useState<ScoringResults | null>(null)
  const [submitted, setSubmitted] = useState(false)

  const handleResponseChange = (itemId: string, value: number | string) => {
    setResponses((prev) => ({ ...prev, [itemId]: value }))
    setErrors((prev) => {
      const next = new Set(prev)
      next.delete(itemId)
      return next
    })
  }

  const handleSubmit = () => {
    const validation = validateResponses(scale, responses)

    if (!validation.valid) {
      setErrors(new Set(validation.missing))
      const firstError = validation.missing[0]
      const element = document.getElementById(`item-${firstError}`)
      element?.scrollIntoView({ behavior: "smooth", block: "center" })
      return
    }

    const adjustedResponses = applyReverseScoring(scale, responses)
    const scoringResults = calculateScores(scale, adjustedResponses)

    setResults(scoringResults)
    setSubmitted(true)
  }

  const handleReset = () => {
    const initial: Responses = {}
    for (const item of scale.items) {
      initial[item.id] = null
    }
    setResponses(initial)
    setErrors(new Set())
    setResults(null)
    setSubmitted(false)
    window.scrollTo({ top: 0, behavior: "smooth" })
  }

  const answeredCount = Object.values(responses).filter((v) => v !== null).length
  const totalCount = scale.items.length

  if (submitted && results) {
    return (
      <div className="space-y-6">
        <ScaleHeader meta={scale.meta} />
        <ScaleResults scale={scale} results={results} />
        <Button onClick={handleReset} variant="outline" className="w-full">
          {language === "ko" ? "다시 검사하기" : "Take Again"}
        </Button>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <ScaleHeader meta={scale.meta} />

      <Card>
        <CardContent className="pt-6">
          <ProgressBar answered={answeredCount} total={totalCount} />

          <div className="space-y-4">
            {scale.items.map((item, index) => (
              <div key={item.id} id={`item-${item.id}`}>
                <ScaleItem
                  item={item}
                  index={index}
                  globalResponseType={scale.responseType}
                  value={responses[item.id]}
                  onChange={(value) => handleResponseChange(item.id, value)}
                  hasError={errors.has(item.id)}
                />
              </div>
            ))}
          </div>

          <div className="mt-8">
            <Button onClick={handleSubmit} className="w-full" size="lg">
              {language === "ko" ? "결과 보기" : "View Results"}
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
