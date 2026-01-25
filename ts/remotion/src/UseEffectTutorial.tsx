import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Sequence,
  Easing,
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
  const y = interpolate(frame - delay, [0, 20], [20, 0], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })
  return <div style={{ opacity, transform: `translateY(${y}px)` }}>{children}</div>
}

// コードブロック（シンタックスハイライト付き）
const CodeBlock: React.FC<{
  code: string
  delay?: number
  highlight?: number[]
}> = ({ code, delay = 0, highlight = [] }) => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const scale = spring({
    frame: frame - delay,
    fps,
    config: { damping: 15 },
  })

  const lines = code.split("\n")

  return (
    <div
      style={{
        transform: `scale(${Math.min(scale, 1)})`,
        background: "#1e1e1e",
        borderRadius: 16,
        padding: "24px 32px",
        fontFamily: "'Fira Code', 'SF Mono', monospace",
        fontSize: 22,
        lineHeight: 1.6,
        boxShadow: "0 20px 60px rgba(0,0,0,0.4)",
        overflow: "hidden",
      }}
    >
      {lines.map((line, i) => (
        <div
          key={i}
          style={{
            background: highlight.includes(i) ? "rgba(97, 175, 239, 0.15)" : "transparent",
            marginLeft: -32,
            marginRight: -32,
            paddingLeft: 32,
            paddingRight: 32,
            borderLeft: highlight.includes(i) ? "3px solid #61afef" : "3px solid transparent",
          }}
        >
          <HighlightedLine line={line} />
        </div>
      ))}
    </div>
  )
}

// シンタックスハイライト
const HighlightedLine: React.FC<{ line: string }> = ({ line }) => {
  const keywords = ["useEffect", "return", "const", "import", "from", "export", "function", "if", "else"]
  const hooks = ["useState", "useEffect", "useCallback", "useMemo", "useRef"]

  let result = line

  // コメント
  if (line.trim().startsWith("//")) {
    return <span style={{ color: "#6a9955" }}>{line}</span>
  }

  const parts: React.ReactNode[] = []
  let remaining = line
  let key = 0

  while (remaining.length > 0) {
    let matched = false

    // 文字列
    const stringMatch = remaining.match(/^(['"`]).*?\1/)
    if (stringMatch) {
      parts.push(<span key={key++} style={{ color: "#ce9178" }}>{stringMatch[0]}</span>)
      remaining = remaining.slice(stringMatch[0].length)
      matched = true
      continue
    }

    // Hooks
    for (const hook of hooks) {
      if (remaining.startsWith(hook)) {
        parts.push(<span key={key++} style={{ color: "#dcdcaa" }}>{hook}</span>)
        remaining = remaining.slice(hook.length)
        matched = true
        break
      }
    }
    if (matched) continue

    // キーワード
    for (const kw of keywords) {
      const regex = new RegExp(`^${kw}\\b`)
      if (regex.test(remaining)) {
        parts.push(<span key={key++} style={{ color: "#c586c0" }}>{kw}</span>)
        remaining = remaining.slice(kw.length)
        matched = true
        break
      }
    }
    if (matched) continue

    // 括弧
    if (/^[{}()\[\]]/.test(remaining)) {
      parts.push(<span key={key++} style={{ color: "#ffd700" }}>{remaining[0]}</span>)
      remaining = remaining.slice(1)
      continue
    }

    // 矢印
    if (remaining.startsWith("=>")) {
      parts.push(<span key={key++} style={{ color: "#569cd6" }}>=&gt;</span>)
      remaining = remaining.slice(2)
      continue
    }

    // デフォルト
    parts.push(<span key={key++} style={{ color: "#d4d4d4" }}>{remaining[0]}</span>)
    remaining = remaining.slice(1)
  }

  return <>{parts}</>
}

// ==================== シーン1: イントロ ====================

const IntroScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const iconScale = spring({ frame, fps, config: { damping: 12 } })
  const titleOpacity = interpolate(frame, [20, 40], [0, 1], { extrapolateRight: "clamp" })

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #20232a 0%, #282c34 100%)",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      {/* Reactロゴ風 */}
      <div style={{ transform: `scale(${iconScale})`, marginBottom: 40 }}>
        <svg width="150" height="150" viewBox="0 0 100 100">
          <ellipse
            cx="50"
            cy="50"
            rx="45"
            ry="18"
            fill="none"
            stroke="#61dafb"
            strokeWidth="2"
            transform="rotate(0, 50, 50)"
          />
          <ellipse
            cx="50"
            cy="50"
            rx="45"
            ry="18"
            fill="none"
            stroke="#61dafb"
            strokeWidth="2"
            transform="rotate(60, 50, 50)"
          />
          <ellipse
            cx="50"
            cy="50"
            rx="45"
            ry="18"
            fill="none"
            stroke="#61dafb"
            strokeWidth="2"
            transform="rotate(120, 50, 50)"
          />
          <circle cx="50" cy="50" r="8" fill="#61dafb" />
        </svg>
      </div>

      <h1
        style={{
          fontSize: 80,
          fontWeight: 700,
          color: "#61dafb",
          opacity: titleOpacity,
          fontFamily: "system-ui",
        }}
      >
        useEffect
      </h1>

      <FadeIn delay={40}>
        <p
          style={{
            fontSize: 32,
            color: "rgba(255,255,255,0.8)",
            marginTop: 20,
            fontFamily: "system-ui",
          }}
        >
          副作用を扱うReact Hook
        </p>
      </FadeIn>
    </AbsoluteFill>
  )
}

// ==================== シーン2: 基本構文 ====================

const BasicSyntaxScene: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `useEffect(() => {
  // 副作用の処理
  console.log("Effect ran!")

  return () => {
    // クリーンアップ処理
  }
}, [dependencies])`

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#61dafb", fontFamily: "system-ui", marginBottom: 40 }}>
          基本構文
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 60 }}>
        <div style={{ flex: 1 }}>
          <FadeIn delay={20}>
            <CodeBlock code={code} delay={20} highlight={[1, 2, 4, 5, 7]} />
          </FadeIn>
        </div>

        <div style={{ flex: 1 }}>
          <FadeIn delay={40}>
            <div style={{ display: "flex", flexDirection: "column", gap: 24 }}>
              <AnnotationBox
                color="#c586c0"
                title="コールバック関数"
                desc="実行したい副作用の処理"
                delay={60}
              />
              <AnnotationBox
                color="#ce9178"
                title="クリーンアップ関数"
                desc="コンポーネント解除時の後片付け"
                delay={80}
              />
              <AnnotationBox
                color="#dcdcaa"
                title="依存配列"
                desc="再実行の条件を指定"
                delay={100}
              />
            </div>
          </FadeIn>
        </div>
      </div>
    </AbsoluteFill>
  )
}

const AnnotationBox: React.FC<{
  color: string
  title: string
  desc: string
  delay: number
}> = ({ color, title, desc, delay }) => {
  const frame = useCurrentFrame()
  const opacity = interpolate(frame - delay, [0, 20], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })

  return (
    <div
      style={{
        opacity,
        background: "rgba(255,255,255,0.05)",
        borderLeft: `4px solid ${color}`,
        padding: "16px 24px",
        borderRadius: "0 8px 8px 0",
      }}
    >
      <h4 style={{ color, fontSize: 24, margin: 0, fontFamily: "system-ui" }}>{title}</h4>
      <p style={{ color: "#9ca3af", fontSize: 20, margin: "8px 0 0", fontFamily: "system-ui" }}>
        {desc}
      </p>
    </div>
  )
}

// ==================== シーン3: 依存配列の挙動 ====================

const DependencyArrayScene: React.FC = () => {
  const frame = useCurrentFrame()

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #0f172a 0%, #1e293b 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#fbbf24", fontFamily: "system-ui", marginBottom: 50 }}>
          依存配列による実行タイミング
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 40, marginTop: 20 }}>
        <DependencyCard
          title="依存配列なし"
          code={`useEffect(() => {
  // 毎回実行
})`}
          timing="毎レンダリング後"
          icon="🔄"
          color="#ef4444"
          delay={20}
        />

        <DependencyCard
          title="空の配列 []"
          code={`useEffect(() => {
  // 初回のみ
}, [])`}
          timing="マウント時のみ"
          icon="1️⃣"
          color="#22c55e"
          delay={50}
        />

        <DependencyCard
          title="依存値あり"
          code={`useEffect(() => {
  // count変更時
}, [count])`}
          timing="依存値の変更時"
          icon="👀"
          color="#3b82f6"
          delay={80}
        />
      </div>

      {/* タイムライン可視化 */}
      <FadeIn delay={110}>
        <div style={{ marginTop: 60 }}>
          <RenderTimeline frame={frame} />
        </div>
      </FadeIn>
    </AbsoluteFill>
  )
}

const DependencyCard: React.FC<{
  title: string
  code: string
  timing: string
  icon: string
  color: string
  delay: number
}> = ({ title, code, timing, icon, color, delay }) => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const scale = spring({ frame: frame - delay, fps, config: { damping: 15 } })

  return (
    <div
      style={{
        transform: `scale(${Math.min(scale, 1)})`,
        background: "rgba(255,255,255,0.05)",
        borderRadius: 16,
        padding: 24,
        flex: 1,
        border: `2px solid ${color}`,
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 16 }}>
        <span style={{ fontSize: 32 }}>{icon}</span>
        <h3 style={{ color: "white", fontSize: 24, margin: 0, fontFamily: "system-ui" }}>{title}</h3>
      </div>

      <div
        style={{
          background: "#1e1e1e",
          borderRadius: 8,
          padding: 16,
          fontFamily: "'Fira Code', monospace",
          fontSize: 16,
          color: "#d4d4d4",
          whiteSpace: "pre",
          marginBottom: 16,
        }}
      >
        {code}
      </div>

      <div
        style={{
          background: color,
          padding: "8px 16px",
          borderRadius: 20,
          display: "inline-block",
        }}
      >
        <span style={{ color: "white", fontSize: 16, fontFamily: "system-ui", fontWeight: 600 }}>
          {timing}
        </span>
      </div>
    </div>
  )
}

// レンダリングタイムライン
const RenderTimeline: React.FC<{ frame: number }> = ({ frame }) => {
  const renders = ["Mount", "Update", "Update", "Unmount"]
  const localFrame = frame - 110

  return (
    <div>
      <p style={{ color: "#9ca3af", fontSize: 20, marginBottom: 16, fontFamily: "system-ui" }}>
        コンポーネントのライフサイクル
      </p>
      <div style={{ display: "flex", alignItems: "center", gap: 0 }}>
        {renders.map((r, i) => {
          const appear = interpolate(localFrame - i * 20, [0, 15], [0, 1], {
            extrapolateLeft: "clamp",
            extrapolateRight: "clamp",
          })

          return (
            <div key={i} style={{ display: "flex", alignItems: "center", opacity: appear }}>
              <div
                style={{
                  background: i === 3 ? "#ef4444" : i === 0 ? "#22c55e" : "#3b82f6",
                  width: 100,
                  height: 50,
                  borderRadius: 8,
                  display: "flex",
                  justifyContent: "center",
                  alignItems: "center",
                  color: "white",
                  fontSize: 16,
                  fontFamily: "system-ui",
                  fontWeight: 600,
                }}
              >
                {r}
              </div>
              {i < renders.length - 1 && (
                <div style={{ width: 60, height: 4, background: "rgba(255,255,255,0.3)" }} />
              )}
            </div>
          )
        })}
      </div>

      {/* 各依存配列での実行タイミング */}
      <div style={{ marginTop: 20, display: "flex", flexDirection: "column", gap: 8 }}>
        <TimelineRow label="なし" dots={[true, true, true, false]} color="#ef4444" frame={localFrame} startDelay={80} />
        <TimelineRow label="[]" dots={[true, false, false, false]} color="#22c55e" frame={localFrame} startDelay={100} />
        <TimelineRow label="[dep]" dots={[true, true, false, false]} color="#3b82f6" frame={localFrame} startDelay={120} />
      </div>
    </div>
  )
}

const TimelineRow: React.FC<{
  label: string
  dots: boolean[]
  color: string
  frame: number
  startDelay: number
}> = ({ label, dots, color, frame, startDelay }) => {
  const appear = interpolate(frame - startDelay, [0, 15], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })

  return (
    <div style={{ display: "flex", alignItems: "center", gap: 16, opacity: appear }}>
      <span style={{ color: "#9ca3af", fontSize: 14, width: 50, fontFamily: "monospace" }}>{label}</span>
      <div style={{ display: "flex", gap: 120 }}>
        {dots.map((active, i) => (
          <div
            key={i}
            style={{
              width: 20,
              height: 20,
              borderRadius: "50%",
              background: active ? color : "rgba(255,255,255,0.1)",
              marginLeft: i === 0 ? 40 : 0,
            }}
          />
        ))}
      </div>
    </div>
  )
}

// ==================== シーン4: クリーンアップ ====================

const CleanupScene: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `useEffect(() => {
  // イベントリスナーを登録
  window.addEventListener('resize', handleResize)

  // クリーンアップ: 解除処理
  return () => {
    window.removeEventListener('resize', handleResize)
  }
}, [])`

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1e3a5f 0%, #0f172a 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#f472b6", fontFamily: "system-ui", marginBottom: 40 }}>
          クリーンアップ関数
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 60 }}>
        <div style={{ flex: 1 }}>
          <FadeIn delay={20}>
            <CodeBlock code={code} delay={20} highlight={[5, 6, 7]} />
          </FadeIn>
        </div>

        <div style={{ flex: 1 }}>
          <FadeIn delay={40}>
            <div style={{ display: "flex", flexDirection: "column", gap: 30 }}>
              <CleanupDiagram frame={frame} />
            </div>
          </FadeIn>

          <FadeIn delay={100}>
            <div style={{ marginTop: 40 }}>
              <h4 style={{ color: "white", fontSize: 24, marginBottom: 16, fontFamily: "system-ui" }}>
                クリーンアップが必要な例
              </h4>
              <ul style={{ color: "#9ca3af", fontSize: 20, lineHeight: 2, fontFamily: "system-ui" }}>
                <li>🎧 イベントリスナー</li>
                <li>⏰ タイマー (setInterval, setTimeout)</li>
                <li>📡 WebSocket接続</li>
                <li>🔔 サブスクリプション</li>
              </ul>
            </div>
          </FadeIn>
        </div>
      </div>
    </AbsoluteFill>
  )
}

const CleanupDiagram: React.FC<{ frame: number }> = ({ frame }) => {
  const localFrame = frame - 40

  const mountAppear = interpolate(localFrame, [0, 20], [0, 1], { extrapolateRight: "clamp" })
  const effectAppear = interpolate(localFrame - 30, [0, 20], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })
  const unmountAppear = interpolate(localFrame - 60, [0, 20], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })
  const cleanupAppear = interpolate(localFrame - 90, [0, 20], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>
      <div style={{ display: "flex", alignItems: "center", gap: 20, opacity: mountAppear }}>
        <div
          style={{
            background: "#22c55e",
            padding: "12px 24px",
            borderRadius: 8,
            color: "white",
            fontSize: 18,
            fontFamily: "system-ui",
          }}
        >
          Mount
        </div>
        <span style={{ color: "#9ca3af", fontSize: 24 }}>→</span>
      </div>

      <div style={{ display: "flex", alignItems: "center", gap: 20, marginLeft: 40, opacity: effectAppear }}>
        <div
          style={{
            background: "#3b82f6",
            padding: "12px 24px",
            borderRadius: 8,
            color: "white",
            fontSize: 18,
            fontFamily: "system-ui",
          }}
        >
          Effect実行 (addEventListener)
        </div>
      </div>

      <div style={{ display: "flex", alignItems: "center", gap: 20, opacity: unmountAppear }}>
        <div
          style={{
            background: "#ef4444",
            padding: "12px 24px",
            borderRadius: 8,
            color: "white",
            fontSize: 18,
            fontFamily: "system-ui",
          }}
        >
          Unmount
        </div>
        <span style={{ color: "#9ca3af", fontSize: 24 }}>→</span>
      </div>

      <div style={{ display: "flex", alignItems: "center", gap: 20, marginLeft: 40, opacity: cleanupAppear }}>
        <div
          style={{
            background: "#f472b6",
            padding: "12px 24px",
            borderRadius: 8,
            color: "white",
            fontSize: 18,
            fontFamily: "system-ui",
          }}
        >
          Cleanup実行 (removeEventListener)
        </div>
      </div>
    </div>
  )
}

// ==================== シーン5: 実践例 ====================

const PracticalExamplesScene: React.FC = () => {
  const frame = useCurrentFrame()

  const examples = [
    {
      title: "データフェッチ",
      code: `useEffect(() => {
  fetch('/api/data')
    .then(res => res.json())
    .then(setData)
}, [])`,
      icon: "📡",
    },
    {
      title: "ドキュメントタイトル",
      code: `useEffect(() => {
  document.title = \`Count: \${count}\`
}, [count])`,
      icon: "📄",
    },
    {
      title: "ローカルストレージ",
      code: `useEffect(() => {
  localStorage.setItem(
    'theme', theme
  )
}, [theme])`,
      icon: "💾",
    },
  ]

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #2d1b4e 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#a78bfa", fontFamily: "system-ui", marginBottom: 50 }}>
          実践的なユースケース
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 30 }}>
        {examples.map((ex, i) => (
          <FadeIn key={i} delay={30 + i * 30}>
            <div
              style={{
                background: "rgba(255,255,255,0.05)",
                borderRadius: 16,
                padding: 24,
                flex: 1,
              }}
            >
              <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 20 }}>
                <span style={{ fontSize: 36 }}>{ex.icon}</span>
                <h3 style={{ color: "white", fontSize: 24, margin: 0, fontFamily: "system-ui" }}>
                  {ex.title}
                </h3>
              </div>

              <div
                style={{
                  background: "#1e1e1e",
                  borderRadius: 12,
                  padding: 20,
                  fontFamily: "'Fira Code', monospace",
                  fontSize: 16,
                  color: "#d4d4d4",
                  whiteSpace: "pre",
                  lineHeight: 1.5,
                }}
              >
                {ex.code}
              </div>
            </div>
          </FadeIn>
        ))}
      </div>
    </AbsoluteFill>
  )
}

// ==================== シーン6: よくある間違い ====================

const CommonMistakesScene: React.FC = () => {
  const frame = useCurrentFrame()

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #450a0a 0%, #1c1917 100%)",
        padding: 80,
      }}
    >
      <FadeIn>
        <h2 style={{ fontSize: 56, color: "#fca5a5", fontFamily: "system-ui", marginBottom: 40 }}>
          ⚠️ よくある間違い
        </h2>
      </FadeIn>

      <div style={{ display: "flex", gap: 40 }}>
        {/* 間違い */}
        <div style={{ flex: 1 }}>
          <FadeIn delay={20}>
            <div
              style={{
                background: "rgba(239, 68, 68, 0.1)",
                border: "2px solid #ef4444",
                borderRadius: 16,
                padding: 24,
              }}
            >
              <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 16 }}>
                <span style={{ fontSize: 28 }}>❌</span>
                <h3 style={{ color: "#fca5a5", fontSize: 24, margin: 0, fontFamily: "system-ui" }}>
                  無限ループ
                </h3>
              </div>

              <div
                style={{
                  background: "#1e1e1e",
                  borderRadius: 8,
                  padding: 16,
                  fontFamily: "'Fira Code', monospace",
                  fontSize: 16,
                  color: "#d4d4d4",
                  whiteSpace: "pre",
                }}
              >
{`useEffect(() => {
  setCount(count + 1)
}) // 依存配列なし!`}
              </div>

              <p style={{ color: "#fca5a5", fontSize: 18, marginTop: 16, fontFamily: "system-ui" }}>
                → 毎回実行 → state更新 → 再レンダー → 毎回実行...
              </p>
            </div>
          </FadeIn>
        </div>

        {/* 正解 */}
        <div style={{ flex: 1 }}>
          <FadeIn delay={60}>
            <div
              style={{
                background: "rgba(34, 197, 94, 0.1)",
                border: "2px solid #22c55e",
                borderRadius: 16,
                padding: 24,
              }}
            >
              <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 16 }}>
                <span style={{ fontSize: 28 }}>✅</span>
                <h3 style={{ color: "#86efac", fontSize: 24, margin: 0, fontFamily: "system-ui" }}>
                  正しい書き方
                </h3>
              </div>

              <div
                style={{
                  background: "#1e1e1e",
                  borderRadius: 8,
                  padding: 16,
                  fontFamily: "'Fira Code', monospace",
                  fontSize: 16,
                  color: "#d4d4d4",
                  whiteSpace: "pre",
                }}
              >
{`useEffect(() => {
  setCount(c => c + 1)
}, []) // 初回のみ実行`}
              </div>

              <p style={{ color: "#86efac", fontSize: 18, marginTop: 16, fontFamily: "system-ui" }}>
                → 空配列で初回のみ実行
              </p>
            </div>
          </FadeIn>
        </div>
      </div>

      <FadeIn delay={100}>
        <div
          style={{
            marginTop: 40,
            background: "rgba(251, 191, 36, 0.1)",
            border: "2px solid #fbbf24",
            borderRadius: 16,
            padding: 24,
          }}
        >
          <h4 style={{ color: "#fbbf24", fontSize: 24, margin: "0 0 12px", fontFamily: "system-ui" }}>
            💡 ポイント: 依存配列には使用する変数を正しく指定しよう
          </h4>
          <p style={{ color: "#fde68a", fontSize: 18, margin: 0, fontFamily: "system-ui" }}>
            ESLint の exhaustive-deps ルールを有効にすると警告してくれます
          </p>
        </div>
      </FadeIn>
    </AbsoluteFill>
  )
}

// ==================== シーン7: まとめ ====================

const SummaryScene: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps, durationInFrames } = useVideoConfig()

  const scale = spring({ frame, fps, config: { damping: 12 } })

  const fadeOut = interpolate(frame, [durationInFrames - 30, durationInFrames], [1, 0], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  })

  const points = [
    "useEffectは副作用を扱うHook",
    "依存配列で実行タイミングを制御",
    "クリーンアップでリソースを解放",
    "無限ループに注意!",
  ]

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #20232a 0%, #282c34 100%)",
        justifyContent: "center",
        alignItems: "center",
        opacity: fadeOut,
      }}
    >
      <div style={{ textAlign: "center", transform: `scale(${scale})` }}>
        <h2
          style={{
            fontSize: 64,
            color: "#61dafb",
            fontFamily: "system-ui",
            marginBottom: 50,
          }}
        >
          まとめ
        </h2>

        <div style={{ display: "flex", flexDirection: "column", gap: 24 }}>
          {points.map((point, i) => (
            <FadeIn key={i} delay={30 + i * 20}>
              <div
                style={{
                  background: "rgba(97, 218, 251, 0.1)",
                  padding: "16px 32px",
                  borderRadius: 12,
                  borderLeft: "4px solid #61dafb",
                }}
              >
                <p style={{ color: "white", fontSize: 28, margin: 0, fontFamily: "system-ui" }}>
                  {point}
                </p>
              </div>
            </FadeIn>
          ))}
        </div>
      </div>
    </AbsoluteFill>
  )
}

// ==================== メインコンポーネント ====================

export const UseEffectTutorial: React.FC = () => {
  return (
    <AbsoluteFill>
      <Sequence from={0} durationInFrames={90}>
        <IntroScene />
      </Sequence>

      <Sequence from={90} durationInFrames={180}>
        <BasicSyntaxScene />
      </Sequence>

      <Sequence from={270} durationInFrames={240}>
        <DependencyArrayScene />
      </Sequence>

      <Sequence from={510} durationInFrames={210}>
        <CleanupScene />
      </Sequence>

      <Sequence from={720} durationInFrames={150}>
        <PracticalExamplesScene />
      </Sequence>

      <Sequence from={870} durationInFrames={180}>
        <CommonMistakesScene />
      </Sequence>

      <Sequence from={1050} durationInFrames={150}>
        <SummaryScene />
      </Sequence>
    </AbsoluteFill>
  )
}
