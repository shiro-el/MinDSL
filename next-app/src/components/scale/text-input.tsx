"use client"

import { Textarea } from "@/components/ui/textarea"
import { useLanguage } from "@/contexts/language-context"

interface TextInputProps {
  itemId: string
  value: string | null
  onChange: (value: string) => void
}

export function TextInput({ itemId, value, onChange }: TextInputProps) {
  const { language } = useLanguage()

  const placeholder =
    language === "ko" ? "답변을 입력하세요..." : "Enter your response..."

  return (
    <Textarea
      id={itemId}
      value={value ?? ""}
      onChange={(e) => onChange(e.target.value)}
      placeholder={placeholder}
      className="min-h-[100px]"
    />
  )
}
