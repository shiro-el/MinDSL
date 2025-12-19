"use client"

import type { Scale, ScoringResults, Language } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { Separator } from "@/components/ui/separator"

interface ScaleResultsProps {
  scale: Scale
  results: ScoringResults
}

const SEMANTIC_COLORS: Record<string, string> = {
  minimal: "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  mild: "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200",
  moderate: "bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200",
  moderately_severe: "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200",
  severe: "bg-red-200 text-red-900 dark:bg-red-800 dark:text-red-100",
  normal: "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  low: "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200",
  high: "bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200",
}

const CATEGORY_LABELS: Record<string, Record<Language, string>> = {
  minimal: { ko: "정상", en: "Minimal" },
  mild: { ko: "경미", en: "Mild" },
  moderate: { ko: "중등도", en: "Moderate" },
  moderately_severe: { ko: "중등도 심함", en: "Moderately Severe" },
  severe: { ko: "심함", en: "Severe" },
  normal: { ko: "정상", en: "Normal" },
  low: { ko: "낮음", en: "Low" },
  high: { ko: "높음", en: "High" },
}

const KEY_LABELS: Record<string, Record<Language, string>> = {
  total: { ko: "총점", en: "Total Score" },
  severity: { ko: "심각도", en: "Severity" },
  state_score: { ko: "상태 점수", en: "State Score" },
  trait_score: { ko: "특성 점수", en: "Trait Score" },
  cognitive_score: { ko: "인지적 점수", en: "Cognitive Score" },
  somatic_score: { ko: "신체적 점수", en: "Somatic Score" },
}

function formatKey(key: string, language: Language): string {
  if (KEY_LABELS[key]) {
    return KEY_LABELS[key][language]
  }
  return key
    .split("_")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ")
}

function formatCategory(value: string, language: Language): string {
  const normalized = value.toLowerCase().replace(/\s+/g, "_")
  if (CATEGORY_LABELS[normalized]) {
    return CATEGORY_LABELS[normalized][language]
  }
  return value
    .split("_")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ")
}

function getCategoryColor(value: string): string {
  const normalized = value.toLowerCase().replace(/\s+/g, "_")
  return SEMANTIC_COLORS[normalized] ?? "bg-secondary text-secondary-foreground"
}

function isCategorical(value: number | string): value is string {
  return typeof value === "string"
}

export function ScaleResults({ scale, results }: ScaleResultsProps) {
  const { language } = useLanguage()

  const numericResults: Array<[string, number]> = []
  const categoricalResults: Array<[string, string]> = []

  for (const [key, value] of Object.entries(results)) {
    if (isCategorical(value)) {
      categoricalResults.push([key, value])
    } else {
      numericResults.push([key, value])
    }
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>{language === "ko" ? "검사 결과" : "Results"}</CardTitle>
        <CardDescription>
          {language === "ko"
            ? "아래는 귀하의 응답을 바탕으로 한 결과입니다."
            : "Below are the results based on your responses."}
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        {numericResults.map(([key, value]) => (
          <div key={key} className="flex items-center justify-between">
            <span className="text-muted-foreground">
              {formatKey(key, language)}
            </span>
            <span className="text-2xl font-bold">{value}</span>
          </div>
        ))}

        {numericResults.length > 0 && categoricalResults.length > 0 && (
          <Separator />
        )}

        {categoricalResults.map(([key, value]) => (
          <div key={key} className="flex items-center justify-between">
            <span className="text-muted-foreground">
              {formatKey(key, language)}
            </span>
            <Badge className={getCategoryColor(value)} variant="secondary">
              {formatCategory(value, language)}
            </Badge>
          </div>
        ))}
      </CardContent>
    </Card>
  )
}
