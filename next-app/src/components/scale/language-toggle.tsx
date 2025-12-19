"use client"

import { useLanguage } from "@/contexts/language-context"
import { Button } from "@/components/ui/button"

export function LanguageToggle() {
  const { language, setLanguage } = useLanguage()

  return (
    <div className="flex gap-1">
      <Button
        variant={language === "ko" ? "default" : "ghost"}
        size="sm"
        onClick={() => setLanguage("ko")}
        className="h-7 px-2 text-xs"
      >
        KO
      </Button>
      <Button
        variant={language === "en" ? "default" : "ghost"}
        size="sm"
        onClick={() => setLanguage("en")}
        className="h-7 px-2 text-xs"
      >
        EN
      </Button>
    </div>
  )
}
