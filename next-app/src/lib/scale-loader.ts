/**
 * MinDSL Scale Loader
 * Auto-scan and load scale JSON files
 */

import type { Scale } from "@/types/mindsl"
import { promises as fs } from "fs"
import path from "path"

export interface ScaleMeta {
  id: string
  name: string
  category: string | null
}

/**
 * Scan the scales directory and return all available scales
 * Server-side only
 */
export async function scanScales(): Promise<ScaleMeta[]> {
  const scalesDir = path.join(process.cwd(), "public", "scales")

  let files: string[]
  try {
    files = await fs.readdir(scalesDir)
  } catch {
    return []
  }

  const scales: ScaleMeta[] = []

  for (const file of files) {
    if (!file.endsWith(".json")) continue

    const id = file.replace(".json", "")
    try {
      const content = await fs.readFile(path.join(scalesDir, file), "utf-8")
      const scale = JSON.parse(content) as Scale

      scales.push({
        id,
        name: scale.name,
        category: scale.meta.category,
      })
    } catch {
      // Skip invalid JSON files
    }
  }

  return scales
}

/**
 * Get available scale IDs (server-side only)
 */
export async function getScaleIds(): Promise<string[]> {
  const scales = await scanScales()
  return scales.map((s) => s.id)
}

/**
 * Load a scale by ID
 * Works in both server and client contexts
 */
export async function getScale(id: string): Promise<Scale> {
  const filePath = `/scales/${id}.json`

  if (typeof window === "undefined") {
    const fullPath = path.join(process.cwd(), "public", filePath)
    const content = await fs.readFile(fullPath, "utf-8")
    return JSON.parse(content) as Scale
  }

  const response = await fetch(filePath)
  if (!response.ok) {
    throw new Error(`Failed to load scale: ${id}`)
  }
  return response.json() as Promise<Scale>
}

/**
 * Get static params for all scales (for generateStaticParams)
 */
export async function generateScaleParams(): Promise<Array<{ id: string }>> {
  const ids = await getScaleIds()
  return ids.map((id) => ({ id }))
}
