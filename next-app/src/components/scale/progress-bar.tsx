"use client"

import { useLanguage } from "@/contexts/language-context"

interface ProgressBarProps {
  answered: number
  total: number
}

export function ProgressBar({ answered, total }: ProgressBarProps) {
  const { language } = useLanguage()
  const progress = Math.round((answered / total) * 100)

  return (
    <div className="mb-6">
      <div className="flex justify-between text-sm text-muted-foreground mb-2">
        <span>
          {language === "ko"
            ? `${answered}/${total} 문항 응답`
            : `${answered}/${total} items answered`}
        </span>
        <span>{progress}%</span>
      </div>
      <div className="h-2 bg-muted rounded-full overflow-hidden">
        <div
          className="h-full bg-primary transition-all duration-300"
          style={{ width: `${progress}%` }}
        />
      </div>
    </div>
  )
}
