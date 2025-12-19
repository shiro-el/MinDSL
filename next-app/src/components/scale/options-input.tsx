"use client"

import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group"
import { Label } from "@/components/ui/label"
import type { Option } from "@/types/mindsl"
import { getLocalizedText } from "@/types/mindsl"
import { useLanguage } from "@/contexts/language-context"
import { cn } from "@/lib/utils"

interface OptionsInputProps {
  itemId: string
  options: Option[]
  value: number | null
  onChange: (value: number) => void
}

export function OptionsInput({
  itemId,
  options,
  value,
  onChange,
}: OptionsInputProps) {
  const { language } = useLanguage()

  return (
    <RadioGroup
      value={value?.toString()}
      onValueChange={(v) => onChange(parseInt(v as string, 10))}
      className="flex flex-col gap-2"
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
            {getLocalizedText(option.label, language)}
          </Label>
        </div>
      ))}
    </RadioGroup>
  )
}
