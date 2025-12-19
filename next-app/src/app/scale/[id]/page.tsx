import { getScale, generateScaleParams } from "@/lib/scale-loader"
import { ScaleView } from "@/components/scale"

export async function generateStaticParams() {
  return await generateScaleParams()
}

interface ScalePageProps {
  params: Promise<{ id: string }>
}

export default async function ScalePage({ params }: ScalePageProps) {
  const { id } = await params
  const scale = await getScale(id)

  return (
    <div className="min-h-screen bg-background">
      <main className="container mx-auto px-4 py-8 max-w-2xl">
        <ScaleView scale={scale} />
      </main>
    </div>
  )
}
