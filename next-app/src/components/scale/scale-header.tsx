"use client"

import type { Meta } from "@/types/mindsl"
import { getLocalizedText } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import { Badge } from "@/components/ui/badge"
import { LanguageToggle } from "./language-toggle"

interface ScaleHeaderProps {
  meta: Meta
}

export function ScaleHeader({ meta }: ScaleHeaderProps) {
  const { language } = useLanguage()
  const name = getLocalizedText(meta.name, language)
  const timeFrame = getLocalizedText(meta.timeFrame, language)

  return (
    <div className="space-y-2">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2">
          <h1 className="text-2xl font-bold tracking-tight">{name}</h1>
          {meta.category && (
            <Badge variant="secondary">{meta.category}</Badge>
          )}
        </div>
        <LanguageToggle />
      </div>
      {timeFrame && (
        <p className="text-muted-foreground">{timeFrame}</p>
      )}
      {meta.citation && (
        <p className="text-xs text-muted-foreground italic">{meta.citation}</p>
      )}
    </div>
  )
}
