"use client"

import type { ResponseType } from "@/types/mindsl"
import { LikertInput } from "./likert-input"
import { OptionsInput } from "./options-input"
import { TextInput } from "./text-input"
import { NumericInput } from "./numeric-input"

interface ResponseInputProps {
  itemId: string
  responseType: ResponseType
  value: number | string | null
  onChange: (value: number | string) => void
}

export function ResponseInput({
  itemId,
  responseType,
  value,
  onChange,
}: ResponseInputProps) {
  switch (responseType.type) {
    case "likert":
      return (
        <LikertInput
          itemId={itemId}
          config={responseType.config}
          value={value as number | null}
          onChange={(v) => onChange(v)}
        />
      )

    case "options":
      return (
        <OptionsInput
          itemId={itemId}
          options={responseType.options}
          value={value as number | null}
          onChange={(v) => onChange(v)}
        />
      )

    case "text":
      return (
        <TextInput
          itemId={itemId}
          value={value as string | null}
          onChange={(v) => onChange(v)}
        />
      )

    case "numeric":
      return (
        <NumericInput
          itemId={itemId}
          min={responseType.min}
          max={responseType.max}
          value={value as number | null}
          onChange={(v) => onChange(v)}
        />
      )

    default:
      return null
  }
}
