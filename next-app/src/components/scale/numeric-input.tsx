"use client"

import { Input } from "@/components/ui/input"
import { useLanguage } from "@/contexts/language-context"

interface NumericInputProps {
  itemId: string
  min: number | null
  max: number | null
  value: number | null
  onChange: (value: number) => void
}

export function NumericInput({
  itemId,
  min,
  max,
  value,
  onChange,
}: NumericInputProps) {
  const { language } = useLanguage()

  const placeholder = (() => {
    if (min !== null && max !== null) {
      return `${min} - ${max}`
    }
    return language === "ko" ? "숫자 입력" : "Enter number"
  })()

  return (
    <Input
      id={itemId}
      type="number"
      value={value ?? ""}
      onChange={(e) => {
        const val = e.target.value === "" ? null : parseInt(e.target.value, 10)
        if (val !== null && !isNaN(val)) onChange(val)
      }}
      min={min ?? undefined}
      max={max ?? undefined}
      placeholder={placeholder}
      className="max-w-[200px]"
    />
  )
}
