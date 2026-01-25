import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Sequence,
} from "remotion"

// ==================== 共通コンポーネント ====================

const FadeIn: React.FC<{ children: React.ReactNode; delay?: number }> = ({
  children,
  delay = 0,
}) => {
  const frame = useCurrentFrame()
  const opacity = interpolate(frame - delay, [0, 20], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })
  const y = interpolate(frame - delay, [0, 20], [30, 0], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })
  return <div style={{ opacity, transform: `translateY(${y}px)` }}>{children}</div>
}

const CodeBlock: React.FC<{ children: string; delay?: number }> = ({
  children,
  delay = 0,
}) => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const scale = spring({
    frame: frame - delay,
    fps,
    config: { damping: 15 },
  })

  return (
    <div
      style={{
        transform: `scale(${Math.min(scale, 1)})`,
        background: "#1e1e1e",
        borderRadius: 12,
        padding: "20px 30px",
        fontFamily: "'Fira Code', monospace",
        fontSize: 28,
        color: "#4ec9b0",
        boxShadow: "0 10px 40px rgba(0,0,0,0.3)",
      }}
    >
      <span style={{ color: "#569cd6" }}>$</span> {children}
    </div>
  )
}

// ==================== シーン1: イントロ ====================

const IntroScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const logoScale = spring({ frame, fps, config: { damping: 12 } })
  const titleOpacity = interpolate(frame, [30, 50], [0, 1], { extrapolateRight: "clamp" })
  const subtitleOpacity = interpolate(frame, [50, 70], [0, 1], { extrapolateRight: "clamp" })

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      {/* Gitロゴ風アイコン */}
      <div style={{ transform: `scale(${logoScale})`, marginBottom: 40 }}>
        <div
          style={{
            width: 150,
            height: 150,
            background: "#f05032",
            borderRadius: 20,
            display: "flex",
            justifyContent: "center",
            alignItems: "center",
            boxShadow: "0 20px 60px rgba(240, 80, 50, 0.4)",
          }}
        >
          <svg width="100" height="100" viewBox="0 0 100 100">
            <circle cx="30" cy="30" r="10" fill="white" />
            <circle cx="70" cy="30" r="10" fill="white" />
            <circle cx="50" cy="70" r="10" fill="white" />
            <line x1="30" y1="40" x2="30" y2="55" stroke="white" strokeWidth="4" />
            <line x1="30" y1="55" x2="50" y2="70" stroke="white" strokeWidth="4" />
            <line x1="70" y1="40" x2="70" y2="55" stroke="white" strokeWidth="4" />
            <line x1="70" y1="55" x2="50" y2="70" stroke="white" strokeWidth="4" />
          </svg>
        </div>
      </div>

      <h1
        style={{
          fontSize: 80,
          fontWeight: 800,
          color: "white",
          opacity: titleOpacity,
          fontFamily: "system-ui",
          textShadow: "0 4px 20px rgba(0,0,0,0.3)",
        }}
      >
        Git & GitHub 入門
      </h1>

      <p
        style={{
          fontSize: 32,
          color: "rgba(255,255,255,0.9)",
          opacity: subtitleOpacity,
          marginTop: 20,
          fontFamily: "system-ui",
        }}
      >
        バージョン管理の基礎を学ぼう
      </p>
    </AbsoluteFill>
  )
}

// ==================== シーン2: Gitとは ====================

const WhatIsGitScene: React.FC = () => {
  const frame = useCurrentFrame()

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2
          style={{
            fontSize: 64,
            color: "#f05032",
            fontFamily: "system-ui",
            marginBottom: 50,
          }}
        >
          Gitとは？
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 60, marginTop: 40 }}>
        {/* 左側：説明 */}
        <div style={{ flex: 1 }}>
          <FadeIn delay={20}>
            <p style={{ fontSize: 36, color: "white", lineHeight: 1.8, fontFamily: "system-ui" }}>
              <span style={{ color: "#4ec9b0" }}>バージョン管理システム</span>
              <br />
              ファイルの変更履歴を記録・管理
            </p>
          </FadeIn>

          <FadeIn delay={40}>
            <div style={{ marginTop: 40 }}>
              <p style={{ fontSize: 28, color: "#9ca3af", lineHeight: 1.8, fontFamily: "system-ui" }}>
                ✓ いつでも過去の状態に戻れる
                <br />
                ✓ 誰が何を変更したか分かる
                <br />
                ✓ 複数人で同時に作業できる
              </p>
            </div>
          </FadeIn>
        </div>

        {/* 右側：図解 */}
        <div style={{ flex: 1, display: "flex", justifyContent: "center", alignItems: "center" }}>
          <FadeIn delay={60}>
            <TimelineVisualization frame={frame} />
          </FadeIn>
        </div>
      </div>
    </AbsoluteFill>
  )
}

// タイムライン可視化
const TimelineVisualization: React.FC<{ frame: number }> = ({ frame }) => {
  const commits = [
    { label: "v1.0", color: "#4ade80" },
    { label: "v1.1", color: "#60a5fa" },
    { label: "v1.2", color: "#f472b6" },
  ]

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 30 }}>
      {commits.map((commit, i) => {
        const delay = i * 15
        const appearFrame = Math.max(0, frame - 60 - delay)
        const opacity = interpolate(appearFrame, [0, 15], [0, 1], { extrapolateRight: "clamp" })
        const x = interpolate(appearFrame, [0, 15], [-50, 0], { extrapolateRight: "clamp" })

        return (
          <div
            key={i}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 20,
              opacity,
              transform: `translateX(${x}px)`,
            }}
          >
            <div
              style={{
                width: 50,
                height: 50,
                borderRadius: "50%",
                background: commit.color,
                display: "flex",
                justifyContent: "center",
                alignItems: "center",
                fontSize: 20,
                color: "white",
                fontWeight: "bold",
              }}
            >
              {i + 1}
            </div>
            <div
              style={{
                background: "rgba(255,255,255,0.1)",
                padding: "12px 24px",
                borderRadius: 8,
                color: "white",
                fontSize: 24,
                fontFamily: "monospace",
              }}
            >
              {commit.label}
            </div>
            {i < commits.length - 1 && (
              <div
                style={{
                  position: "absolute",
                  left: 25,
                  top: 50 + i * 80,
                  width: 4,
                  height: 30,
                  background: "rgba(255,255,255,0.3)",
                }}
              />
            )}
          </div>
        )
      })}
    </div>
  )
}

// ==================== シーン3: 基本コマンド ====================

const BasicCommandsScene: React.FC = () => {
  const frame = useCurrentFrame()

  const commands = [
    { cmd: "git init", desc: "リポジトリを初期化" },
    { cmd: "git add .", desc: "変更をステージング" },
    { cmd: "git commit -m 'message'", desc: "変更を記録" },
    { cmd: "git push", desc: "リモートに送信" },
  ]

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #0f172a 0%, #1e293b 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 64, color: "#4ade80", fontFamily: "system-ui", marginBottom: 50 }}>
          基本コマンド
        </h2>
      </FadeIn>

      <div style={{ display: "flex", flexDirection: "column", gap: 30, marginTop: 20 }}>
        {commands.map((item, i) => {
          const delay = 20 + i * 25
          const appearFrame = Math.max(0, frame - delay)
          const opacity = interpolate(appearFrame, [0, 20], [0, 1], { extrapolateRight: "clamp" })
          const x = interpolate(appearFrame, [0, 20], [-100, 0], { extrapolateRight: "clamp" })

          return (
            <div
              key={i}
              style={{
                display: "flex",
                alignItems: "center",
                gap: 40,
                opacity,
                transform: `translateX(${x}px)`,
              }}
            >
              <div
                style={{
                  background: "#1e1e1e",
                  padding: "16px 32px",
                  borderRadius: 12,
                  fontFamily: "'Fira Code', monospace",
                  fontSize: 28,
                  color: "#4ec9b0",
                  minWidth: 450,
                  boxShadow: "0 4px 20px rgba(0,0,0,0.3)",
                }}
              >
                <span style={{ color: "#569cd6" }}>$</span> {item.cmd}
              </div>
              <span style={{ color: "#9ca3af", fontSize: 28, fontFamily: "system-ui" }}>
                → {item.desc}
              </span>
            </div>
          )
        })}
      </div>

      {/* ワークフロー図 */}
      <FadeIn delay={120}>
        <div
          style={{
            marginTop: 60,
            display: "flex",
            justifyContent: "center",
            alignItems: "center",
            gap: 20,
          }}
        >
          {["Working Dir", "Staging", "Repository", "Remote"].map((stage, i) => (
            <div key={i} style={{ display: "flex", alignItems: "center", gap: 20 }}>
              <div
                style={{
                  background: i === 3 ? "#f05032" : "rgba(255,255,255,0.1)",
                  padding: "16px 24px",
                  borderRadius: 8,
                  color: "white",
                  fontSize: 20,
                  fontFamily: "system-ui",
                }}
              >
                {stage}
              </div>
              {i < 3 && <span style={{ color: "#4ade80", fontSize: 28 }}>→</span>}
            </div>
          ))}
        </div>
      </FadeIn>
    </AbsoluteFill>
  )
}

// ==================== シーン4: ブランチ ====================

const BranchScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #2d1b4e 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 64, color: "#a78bfa", fontFamily: "system-ui", marginBottom: 30 }}>
          ブランチ（Branch）
        </h2>
        <p style={{ fontSize: 28, color: "#9ca3af", fontFamily: "system-ui" }}>
          メインの開発ラインから分岐して作業できる
        </p>
      </FadeIn>

      {/* ブランチ図 */}
      <div style={{ marginTop: 60, position: "relative", height: 400 }}>
        <BranchVisualization frame={frame} />
      </div>

      <FadeIn delay={120}>
        <div style={{ display: "flex", gap: 40, marginTop: 40 }}>
          <CodeBlock delay={0}>git branch feature</CodeBlock>
          <CodeBlock delay={20}>git checkout feature</CodeBlock>
          <CodeBlock delay={40}>git merge feature</CodeBlock>
        </div>
      </FadeIn>
    </AbsoluteFill>
  )
}

const BranchVisualization: React.FC<{ frame: number }> = ({ frame }) => {
  // メインブランチのコミット
  const mainCommits = [
    { x: 100, label: "1" },
    { x: 250, label: "2" },
    { x: 400, label: "3" },
    { x: 700, label: "5" },
  ]

  // featureブランチのコミット
  const featureCommits = [
    { x: 400, y: 150, label: "a" },
    { x: 550, y: 150, label: "b" },
  ]

  const branchAppear = interpolate(frame, [40, 60], [0, 1], { extrapolateRight: "clamp" })
  const mergeAppear = interpolate(frame, [80, 100], [0, 1], { extrapolateRight: "clamp" })

  return (
    <svg width="900" height="300" style={{ marginLeft: 100 }}>
      {/* メインブランチライン */}
      <line x1="50" y1="80" x2="750" y2="80" stroke="#4ade80" strokeWidth="4" />

      {/* フィーチャーブランチライン */}
      <g style={{ opacity: branchAppear }}>
        <path
          d="M 400 80 Q 400 115 430 150"
          stroke="#a78bfa"
          strokeWidth="4"
          fill="none"
        />
        <line x1="430" y1="150" x2="580" y2="150" stroke="#a78bfa" strokeWidth="4" />
      </g>

      {/* マージライン */}
      <g style={{ opacity: mergeAppear }}>
        <path
          d="M 580 150 Q 640 150 700 80"
          stroke="#a78bfa"
          strokeWidth="4"
          fill="none"
        />
      </g>

      {/* メインコミット */}
      {mainCommits.map((c, i) => {
        const delay = i * 10
        const appear = interpolate(frame - delay, [0, 15], [0, 1], { extrapolateRight: "clamp" })
        return (
          <g key={i} style={{ opacity: appear }}>
            <circle cx={c.x} cy={80} r={25} fill="#4ade80" />
            <text x={c.x} y={88} textAnchor="middle" fill="white" fontSize="20" fontWeight="bold">
              {c.label}
            </text>
          </g>
        )
      })}

      {/* フィーチャーコミット */}
      {featureCommits.map((c, i) => {
        const appear = interpolate(frame - 50 - i * 15, [0, 15], [0, 1], {
          extrapolateLeft: "clamp",
          extrapolateRight: "clamp",
        })
        return (
          <g key={i} style={{ opacity: appear }}>
            <circle cx={c.x + 30 + i * 120} cy={c.y} r={25} fill="#a78bfa" />
            <text
              x={c.x + 30 + i * 120}
              y={c.y + 8}
              textAnchor="middle"
              fill="white"
              fontSize="20"
              fontWeight="bold"
            >
              {c.label}
            </text>
          </g>
        )
      })}

      {/* ラベル */}
      <text x="50" y="40" fill="#4ade80" fontSize="24" fontFamily="system-ui">
        main
      </text>
      <g style={{ opacity: branchAppear }}>
        <text x="500" y="200" fill="#a78bfa" fontSize="24" fontFamily="system-ui">
          feature
        </text>
      </g>
    </svg>
  )
}

// ==================== シーン5: GitHub ====================

const GitHubScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const iconScale = spring({ frame, fps, config: { damping: 12 } })

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #0d1117 0%, #161b22 100%)",
        padding: 80,
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 30, marginBottom: 50 }}>
        <div style={{ transform: `scale(${iconScale})` }}>
          <svg width="80" height="80" viewBox="0 0 100 100">
            <circle cx="50" cy="50" r="45" fill="white" />
            <path
              d="M50 10 C25 10 10 30 10 50 C10 70 25 85 45 90 C47 90 48 89 48 87 L48 80 C35 82 32 75 32 75 C30 70 27 68 27 68 C22 65 28 65 28 65 C33 65 36 70 36 70 C42 78 50 76 53 74 C53 70 55 68 57 67 C45 65 33 60 33 45 C33 40 35 36 38 33 C37 32 35 27 40 20 C40 20 45 18 55 25 C58 24 62 24 66 25 C75 18 80 20 80 20 C85 27 82 32 82 33 C85 36 87 40 87 45 C87 60 75 65 63 67 C66 70 68 74 68 80 L68 87 C68 89 69 90 72 90 C92 85 100 70 100 50 C100 30 75 10 50 10"
              fill="#0d1117"
            />
          </svg>
        </div>
        <h2 style={{ fontSize: 64, color: "white", fontFamily: "system-ui" }}>GitHub</h2>
      </div>

      <FadeIn delay={20}>
        <p style={{ fontSize: 32, color: "#9ca3af", marginBottom: 50, fontFamily: "system-ui" }}>
          Gitリポジトリをクラウドで管理・共有できるサービス
        </p>
      </FadeIn>

      <div style={{ display: "flex", gap: 40, flexWrap: "wrap" }}>
        {[
          { icon: "📦", title: "リポジトリ", desc: "コードを保存・公開" },
          { icon: "🔀", title: "Pull Request", desc: "変更をレビュー・マージ" },
          { icon: "🐛", title: "Issues", desc: "タスク・バグを管理" },
          { icon: "🤝", title: "Collaboration", desc: "チームで共同開発" },
        ].map((item, i) => (
          <FadeIn key={i} delay={40 + i * 20}>
            <div
              style={{
                background: "rgba(255,255,255,0.05)",
                border: "1px solid rgba(255,255,255,0.1)",
                borderRadius: 16,
                padding: "30px 40px",
                width: 350,
              }}
            >
              <span style={{ fontSize: 48 }}>{item.icon}</span>
              <h3 style={{ fontSize: 28, color: "white", margin: "16px 0 8px", fontFamily: "system-ui" }}>
                {item.title}
              </h3>
              <p style={{ fontSize: 20, color: "#9ca3af", fontFamily: "system-ui" }}>{item.desc}</p>
            </div>
          </FadeIn>
        ))}
      </div>
    </AbsoluteFill>
  )
}

// ==================== シーン6: PRフロー ====================

const PullRequestScene: React.FC = () => {
  const frame = useCurrentFrame()

  const steps = [
    { num: 1, title: "Fork / Clone", desc: "リポジトリをコピー" },
    { num: 2, title: "Branch", desc: "ブランチを作成" },
    { num: 3, title: "Commit", desc: "変更をコミット" },
    { num: 4, title: "Push", desc: "リモートに送信" },
    { num: 5, title: "Pull Request", desc: "マージをリクエスト" },
    { num: 6, title: "Merge", desc: "本流に統合" },
  ]

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1e3a5f 0%, #0d1117 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#58a6ff", fontFamily: "system-ui", marginBottom: 50 }}>
          Pull Request の流れ
        </h2>
      </FadeIn>

      <div
        style={{
          display: "flex",
          flexWrap: "wrap",
          gap: 30,
          justifyContent: "center",
          marginTop: 30,
        }}
      >
        {steps.map((step, i) => {
          const delay = 20 + i * 15
          const appear = interpolate(frame - delay, [0, 20], [0, 1], {
            extrapolateLeft: "clamp",
            extrapolateRight: "clamp",
          })
          const y = interpolate(frame - delay, [0, 20], [30, 0], {
            extrapolateLeft: "clamp",
            extrapolateRight: "clamp",
          })

          return (
            <div
              key={i}
              style={{
                opacity: appear,
                transform: `translateY(${y}px)`,
                display: "flex",
                alignItems: "center",
                gap: 20,
              }}
            >
              <div
                style={{
                  background: "linear-gradient(135deg, #238636 0%, #2ea043 100%)",
                  width: 60,
                  height: 60,
                  borderRadius: "50%",
                  display: "flex",
                  justifyContent: "center",
                  alignItems: "center",
                  fontSize: 28,
                  fontWeight: "bold",
                  color: "white",
                }}
              >
                {step.num}
              </div>
              <div>
                <h4 style={{ fontSize: 24, color: "white", margin: 0, fontFamily: "system-ui" }}>
                  {step.title}
                </h4>
                <p style={{ fontSize: 18, color: "#8b949e", margin: 0, fontFamily: "system-ui" }}>
                  {step.desc}
                </p>
              </div>
              {i < steps.length - 1 && i !== 2 && (
                <span style={{ color: "#58a6ff", fontSize: 32, marginLeft: 10 }}>→</span>
              )}
            </div>
          )
        })}
      </div>
    </AbsoluteFill>
  )
}

// ==================== シーン7: まとめ ====================

const SummaryScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps, durationInFrames } = useVideoConfig()

  const scale = spring({ frame, fps, config: { damping: 12 } })

  // フェードアウト
  const fadeOut = interpolate(frame, [durationInFrames - 30, durationInFrames], [1, 0], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
        justifyContent: "center",
        alignItems: "center",
        opacity: fadeOut,
      }}
    >
      <div style={{ textAlign: "center", transform: `scale(${scale})` }}>
        <h2
          style={{
            fontSize: 72,
            color: "white",
            fontFamily: "system-ui",
            marginBottom: 30,
            textShadow: "0 4px 20px rgba(0,0,0,0.3)",
          }}
        >
          Let's Git Started! 🚀
        </h2>

        <FadeIn delay={30}>
          <div
            style={{
              display: "flex",
              gap: 30,
              justifyContent: "center",
              marginTop: 40,
            }}
          >
            {[
              { icon: "📚", text: "git init から始めよう" },
              { icon: "🌿", text: "ブランチを活用しよう" },
              { icon: "🤝", text: "GitHubで共有しよう" },
            ].map((item, i) => (
              <FadeIn key={i} delay={50 + i * 20}>
                <div
                  style={{
                    background: "rgba(255,255,255,0.2)",
                    padding: "24px 36px",
                    borderRadius: 16,
                    color: "white",
                    fontSize: 24,
                    fontFamily: "system-ui",
                  }}
                >
                  {item.icon} {item.text}
                </div>
              </FadeIn>
            ))}
          </div>
        </FadeIn>
      </div>
    </AbsoluteFill>
  )
}

// ==================== メインコンポーネント ====================

export const GitTutorial: React.FC = () => {
  return (
    <AbsoluteFill>
      <Sequence from={0} durationInFrames={120}>
        <IntroScene />
      </Sequence>

      <Sequence from={120} durationInFrames={180}>
        <WhatIsGitScene />
      </Sequence>

      <Sequence from={300} durationInFrames={180}>
        <BasicCommandsScene />
      </Sequence>

      <Sequence from={480} durationInFrames={180}>
        <BranchScene />
      </Sequence>

      <Sequence from={660} durationInFrames={150}>
        <GitHubScene />
      </Sequence>

      <Sequence from={810} durationInFrames={150}>
        <PullRequestScene />
      </Sequence>

      <Sequence from={960} durationInFrames={140}>
        <SummaryScene />
      </Sequence>
    </AbsoluteFill>
  )
}
