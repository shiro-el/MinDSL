"use client"

import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group"
import { Label } from "@/components/ui/label"
import type { LikertConfig } from "@/types/mindsl"
import { getLocalizedText } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import { cn } from "@/lib/utils"

interface LikertInputProps {
  itemId: string
  config: LikertConfig
  value: number | null
  onChange: (value: number) => void
}

export function LikertInput({
  itemId,
  config,
  value,
  onChange,
}: LikertInputProps) {
  const { language } = useLanguage()
  const options: Array<{ value: number; label: string }> = []

  for (let i = config.min; i <= config.max; i++) {
    const labelIndex = i - config.min
    const label = config.labels?.[labelIndex]
      ? getLocalizedText(config.labels[labelIndex], language)
      : String(i)
    options.push({ value: i, label })
  }

  return (
    <RadioGroup
      value={value?.toString()}
      onValueChange={(v) => onChange(parseInt(v as string, 10))}
      className="flex flex-col gap-2 sm:flex-row sm:flex-wrap sm:gap-4"
    >
      {options.map((option) => (
        <div key={option.value} className="flex items-center gap-2">
          <RadioGroupItem
            value={option.value.toString()}
            id={`${itemId}-${option.value}`}
          />
          <Label
            htmlFor={`${itemId}-${option.value}`}
            className={cn(
              "text-sm font-normal cursor-pointer",
              value === option.value && "font-medium text-primary"
            )}
          >
            <span className="text-muted-foreground mr-1">({option.value})</span>
            {option.label}
          </Label>
        </div>
      ))}
    </RadioGroup>
  )
}
