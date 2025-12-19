"use client"

import type { Item, ResponseType } from "@/types/mindsl"
import { getLocalizedText } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import { ResponseInput } from "./response-input"
import { cn } from "@/lib/utils"

interface ScaleItemProps {
  item: Item
  index: number
  globalResponseType: ResponseType
  value: number | string | null
  onChange: (value: number | string) => void
  hasError?: boolean
}

export function ScaleItem({
  item,
  index,
  globalResponseType,
  value,
  onChange,
  hasError = false,
}: ScaleItemProps) {
  const { language } = useLanguage()
  const text = getLocalizedText(item.text, language)
  const responseType = item.responseType ?? globalResponseType

  return (
    <div
      className={cn(
        "space-y-3 p-4 rounded-lg transition-colors",
        hasError && "bg-destructive/5 ring-1 ring-destructive/20",
        value !== null && !hasError && "bg-muted/30"
      )}
    >
      <div className="flex gap-3">
        <span className="text-muted-foreground font-medium min-w-[2rem]">
          {index + 1}.
        </span>
        <p className="text-foreground leading-relaxed">{text}</p>
      </div>

      <div className="pl-10">
        <ResponseInput
          itemId={item.id}
          responseType={responseType}
          value={value}
          onChange={onChange}
        />
      </div>

      {hasError && (
        <p className="pl-10 text-sm text-destructive">
          {language === "ko" ? "응답이 필요합니다" : "Response required"}
        </p>
      )}
    </div>
  )
}
