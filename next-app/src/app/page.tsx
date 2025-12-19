import Link from "next/link"
import { scanScales, getScale } from "@/lib/scale-loader"
import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"

export default async function Page() {
  const scaleMetas = await scanScales()

  const scales = await Promise.all(
    scaleMetas.map(async (meta) => {
      const scale = await getScale(meta.id)
      return { ...meta, scale }
    })
  )

  return (
    <div className="min-h-screen bg-background">
      <main className="container mx-auto px-4 py-16 max-w-4xl">
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold tracking-tight mb-4">MinDSL</h1>
          <p className="text-xl text-muted-foreground">
            심리 평가 도구를 위한 도메인 특화 언어
          </p>
        </div>

        <div className="grid gap-6 md:grid-cols-2">
          {scales.map(({ id, scale }) => (
            <Card key={id}>
              <CardHeader>
                <div className="flex items-center gap-2">
                  <CardTitle>{scale.name}</CardTitle>
                  {scale.meta.category && (
                    <Badge variant="secondary">{scale.meta.category}</Badge>
                  )}
                </div>
                <CardDescription>
                  {scale.meta.name.ko ?? scale.meta.name.en}
                </CardDescription>
              </CardHeader>
              <CardContent>
                <p className="text-sm text-muted-foreground mb-4">
                  {scale.items.length}개 문항
                  {scale.meta.timeFrame?.ko && ` | ${scale.meta.timeFrame.ko}`}
                </p>
                <Link href={`/scale/${id}`} className="w-full">
                  <Button className="w-full">검사 시작</Button>
                </Link>
              </CardContent>
            </Card>
          ))}

          {scales.length === 0 && (
            <div className="col-span-2 text-center text-muted-foreground py-8">
              아직 등록된 척도가 없습니다.
            </div>
          )}
        </div>
      </main>
    </div>
  )
}
