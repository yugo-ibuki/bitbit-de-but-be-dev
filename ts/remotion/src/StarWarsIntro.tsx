import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Easing,
} from "remotion"

// 星を生成
const generateStars = (count: number) => {
  return Array.from({ length: count }, (_, i) => ({
    x: (i * 173.5) % 100,
    y: (i * 91.7) % 100,
    size: 1 + (i % 3),
    twinkleOffset: i * 0.5,
  }))
}

const stars = generateStars(200)

// スターウォーズ風のクロールテキスト
const crawlText = `
A long time ago in a development environment
far, far away....





DOT-CLAUDE-SYNC


Episode I
THE WORKTREE MENACE

It is a period of productivity crisis.
Developers working with Claude Code
struggle to share their precious
.claude directories across git worktrees.

Chaos spreads across workspaces.
Prompts, commands, and skills
remain isolated in separate projects,
forcing developers to manually copy
files between repositories.

But hope emerges from the shadows.
A new CLI tool rises to bring
balance to the workflow...





FEATURES

✦ Sync .claude directories across projects
✦ Auto-detect git worktrees
✦ Priority-based conflict resolution
✦ Dry-run mode for safety
✦ Simple YAML configuration





COMMANDS

init     Initialize configuration
push     Sync files across projects
detect   Auto-detect worktrees
list     Show groups and details
rm       Delete files from all projects
mv       Move/rename files





INSTALLATION

go install github.com/yugo-ibuki/
    dot-claude-sync@latest





github.com/yugo-ibuki/dot-claude-sync



Created by Ugo
`.trim()

// イントロテキスト "A long time ago..."
const IntroText: React.FC = () => {
  const frame = useCurrentFrame()

  const opacity = interpolate(frame, [0, 30, 120, 150], [0, 1, 1, 0], {
    extrapolateRight: "clamp",
  })

  return (
    <div
      style={{
        position: "absolute",
        width: "100%",
        height: "100%",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        opacity,
      }}
    >
      <p
        style={{
          color: "#4fd5d6",
          fontSize: 42,
          fontFamily: "system-ui, sans-serif",
          textAlign: "center",
          lineHeight: 1.8,
        }}
      >
        A long time ago in a development environment
        <br />
        far, far away....
      </p>
    </div>
  )
}

// メインロゴ
const MainLogo: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const logoStart = 150
  const localFrame = frame - logoStart

  if (localFrame < 0) return null

  // ロゴが縮小していく
  const scale = interpolate(localFrame, [0, 120], [3, 0.3], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
    easing: Easing.out(Easing.cubic),
  })

  const opacity = interpolate(localFrame, [0, 30, 90, 120], [0, 1, 1, 0], {
    extrapolateRight: "clamp",
  })

  return (
    <div
      style={{
        position: "absolute",
        width: "100%",
        height: "100%",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        opacity,
      }}
    >
      <h1
        style={{
          color: "#ffd700",
          fontSize: 120,
          fontFamily: "system-ui, sans-serif",
          fontWeight: 900,
          textAlign: "center",
          transform: `scale(${scale})`,
          textShadow: "0 0 60px rgba(255, 215, 0, 0.5)",
          letterSpacing: 8,
        }}
      >
        DOT-CLAUDE
        <br />
        SYNC
      </h1>
    </div>
  )
}

// スクロールするテキスト
const CrawlText: React.FC = () => {
  const frame = useCurrentFrame()
  const { durationInFrames } = useVideoConfig()

  const crawlStart = 270

  if (frame < crawlStart) return null

  const localFrame = frame - crawlStart

  // スクロール量（下から上へ）
  const scrollY = interpolate(localFrame, [0, durationInFrames - crawlStart], [1200, -3500], {
    extrapolateRight: "clamp",
  })

  // フェードイン
  const opacity = interpolate(localFrame, [0, 60], [0, 1], {
    extrapolateRight: "clamp",
  })

  const lines = crawlText.split("\n").slice(6) // "A long time ago..."をスキップ

  return (
    <div
      style={{
        position: "absolute",
        width: "100%",
        height: "100%",
        perspective: 400,
        perspectiveOrigin: "50% 100%",
        overflow: "hidden",
        opacity,
      }}
    >
      <div
        style={{
          position: "absolute",
          width: "60%",
          left: "20%",
          transformStyle: "preserve-3d",
          transform: `rotateX(25deg) translateY(${scrollY}px)`,
        }}
      >
        {lines.map((line, i) => {
          const isTitle =
            line.includes("DOT-CLAUDE-SYNC") ||
            line.includes("Episode") ||
            line.includes("MENACE") ||
            line.includes("FEATURES") ||
            line.includes("COMMANDS") ||
            line.includes("INSTALLATION")
          const isSymbol = line.includes("✦")
          const isCommand =
            line.startsWith("init") ||
            line.startsWith("push") ||
            line.startsWith("detect") ||
            line.startsWith("list") ||
            line.startsWith("rm") ||
            line.startsWith("mv")
          const isCredit = line.includes("Created by") || line.includes("github.com")

          return (
            <p
              key={i}
              style={{
                color: "#ffd700",
                fontSize: isTitle ? 56 : isCredit ? 36 : 32,
                fontFamily: "system-ui, sans-serif",
                fontWeight: isTitle ? 700 : 400,
                textAlign: "center",
                margin: 0,
                lineHeight: line === "" ? 0.8 : 1.6,
                minHeight: line === "" ? 20 : "auto",
                textShadow: "0 0 10px rgba(255, 215, 0, 0.3)",
                letterSpacing: isTitle ? 4 : 1,
              }}
            >
              {line || "\u00A0"}
            </p>
          )
        })}
      </div>
    </div>
  )
}

// 星空背景
const StarField: React.FC = () => {
  const frame = useCurrentFrame()

  return (
    <AbsoluteFill style={{ backgroundColor: "#000" }}>
      {stars.map((star, i) => {
        const twinkle = Math.sin(frame * 0.05 + star.twinkleOffset) * 0.3 + 0.7
        return (
          <div
            key={i}
            style={{
              position: "absolute",
              left: `${star.x}%`,
              top: `${star.y}%`,
              width: star.size,
              height: star.size,
              borderRadius: "50%",
              backgroundColor: "white",
              opacity: twinkle,
            }}
          />
        )
      })}
    </AbsoluteFill>
  )
}

export const StarWarsIntro: React.FC = () => {
  return (
    <AbsoluteFill>
      <StarField />
      <IntroText />
      <MainLogo />
      <CrawlText />
    </AbsoluteFill>
  )
}
