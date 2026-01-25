import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
  Sequence,
} from "remotion"

// ==================== タイピングアニメーション ====================

const TypedCode: React.FC<{
  code: string
  startFrame: number
  speed?: number
}> = ({ code, startFrame, speed = 2 }) => {
  const frame = useCurrentFrame()
  const localFrame = Math.max(0, frame - startFrame)

  const visibleChars = Math.min(Math.floor(localFrame / speed), code.length)
  const displayCode = code.slice(0, visibleChars)
  const showCursor = localFrame < code.length * speed + 30 && Math.floor(localFrame / 8) % 2 === 0

  return (
    <div style={{ position: "relative" }}>
      <HighlightedCode code={displayCode} />
      {showCursor && (
        <span
          style={{
            backgroundColor: "#ffd700",
            width: 2,
            height: 24,
            display: "inline-block",
            marginLeft: 2,
            verticalAlign: "middle",
          }}
        />
      )}
    </div>
  )
}

// シンタックスハイライト
const HighlightedCode: React.FC<{ code: string }> = ({ code }) => {
  const lines = code.split("\n")

  return (
    <pre
      style={{
        margin: 0,
        fontFamily: "'Fira Code', 'SF Mono', monospace",
        fontSize: 18,
        lineHeight: 1.6,
        color: "#abb2bf",
      }}
    >
      {lines.map((line, i) => (
        <div key={i}>
          <LineHighlight line={line} />
        </div>
      ))}
    </pre>
  )
}

const LineHighlight: React.FC<{ line: string }> = ({ line }) => {
  // シンプルなハイライト
  let result = line
    .replace(/(useEffect|useState|useRef)/g, '<span class="hook">$1</span>')
    .replace(/(const|let|return|import|from|export)/g, '<span class="keyword">$1</span>')
    .replace(/('.*?'|".*?")/g, '<span class="string">$1</span>')
    .replace(/(\d+)/g, '<span class="number">$1</span>')
    .replace(/(\/\/.*)/g, '<span class="comment">$1</span>')

  const styles: Record<string, string> = {
    hook: "#61afef",
    keyword: "#c678dd",
    string: "#98c379",
    number: "#d19a66",
    comment: "#5c6370",
  }

  // HTMLパース風の処理（簡易版）
  const parts: React.ReactNode[] = []
  let remaining = line
  let key = 0

  const patterns: [RegExp, string][] = [
    [/^(useEffect|useState|useRef|setCount|setData)/, styles.hook],
    [/^(const|let|return|import|from|export|function|if|else)(?=\s|$|\(|\{)/, styles.keyword],
    [/^('.*?'|".*?"|`.*?`)/, styles.string],
    [/^(\d+)/, styles.number],
    [/^(\/\/.*)/, styles.comment],
    [/^(=>)/, "#56b6c2"],
    [/^([{}()\[\]])/, "#ffd700"],
  ]

  while (remaining.length > 0) {
    let matched = false

    for (const [pattern, color] of patterns) {
      const match = remaining.match(pattern)
      if (match) {
        parts.push(
          <span key={key++} style={{ color }}>
            {match[0]}
          </span>
        )
        remaining = remaining.slice(match[0].length)
        matched = true
        break
      }
    }

    if (!matched) {
      parts.push(
        <span key={key++} style={{ color: "#abb2bf" }}>
          {remaining[0]}
        </span>
      )
      remaining = remaining.slice(1)
    }
  }

  return <>{parts}</>
}

// ==================== 挙動ビジュアライザー ====================

// コンポーネント状態の可視化
const ComponentVisualizer: React.FC<{
  count: number
  logs: string[]
  phase: "mount" | "update" | "unmount" | "idle"
  effectRunning: boolean
}> = ({ count, logs, phase, effectRunning }) => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const phaseColors = {
    mount: "#22c55e",
    update: "#3b82f6",
    unmount: "#ef4444",
    idle: "#6b7280",
  }

  const phaseLabels = {
    mount: "🟢 Mount",
    update: "🔵 Update",
    unmount: "🔴 Unmount",
    idle: "⚪ Idle",
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 24, height: "100%" }}>
      {/* ブラウザ風プレビュー */}
      <div
        style={{
          background: "#ffffff",
          borderRadius: 12,
          overflow: "hidden",
          boxShadow: "0 10px 40px rgba(0,0,0,0.3)",
          flex: 1,
        }}
      >
        {/* ブラウザバー */}
        <div
          style={{
            background: "#e5e7eb",
            padding: "12px 16px",
            display: "flex",
            alignItems: "center",
            gap: 8,
          }}
        >
          <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#ef4444" }} />
          <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#fbbf24" }} />
          <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#22c55e" }} />
          <div
            style={{
              marginLeft: 16,
              background: "white",
              borderRadius: 6,
              padding: "6px 16px",
              fontSize: 14,
              color: "#6b7280",
              flex: 1,
            }}
          >
            localhost:3000
          </div>
        </div>

        {/* アプリ表示 */}
        <div
          style={{
            padding: 40,
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            justifyContent: "center",
            minHeight: 200,
          }}
        >
          <div
            style={{
              fontSize: 64,
              fontWeight: 700,
              color: "#1f2937",
              fontFamily: "system-ui",
            }}
          >
            {count}
          </div>
          <button
            style={{
              marginTop: 20,
              padding: "12px 32px",
              fontSize: 18,
              background: "#3b82f6",
              color: "white",
              border: "none",
              borderRadius: 8,
              cursor: "pointer",
              fontFamily: "system-ui",
            }}
          >
            + Increment
          </button>
        </div>
      </div>

      {/* ステータスパネル */}
      <div
        style={{
          background: "rgba(255,255,255,0.05)",
          borderRadius: 12,
          padding: 20,
        }}
      >
        {/* フェーズ表示 */}
        <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 16 }}>
          <div
            style={{
              background: phaseColors[phase],
              padding: "8px 16px",
              borderRadius: 20,
              color: "white",
              fontSize: 16,
              fontWeight: 600,
              fontFamily: "system-ui",
            }}
          >
            {phaseLabels[phase]}
          </div>

          {effectRunning && (
            <div
              style={{
                background: "#8b5cf6",
                padding: "8px 16px",
                borderRadius: 20,
                color: "white",
                fontSize: 16,
                fontWeight: 600,
                fontFamily: "system-ui",
                animation: "pulse 1s infinite",
              }}
            >
              ⚡ Effect Running
            </div>
          )}
        </div>

        {/* コンソールログ */}
        <div
          style={{
            background: "#1e1e1e",
            borderRadius: 8,
            padding: 12,
            fontFamily: "'Fira Code', monospace",
            fontSize: 14,
            maxHeight: 150,
            overflow: "hidden",
          }}
        >
          <div style={{ color: "#6b7280", marginBottom: 8 }}>Console:</div>
          {logs.map((log, i) => (
            <div
              key={i}
              style={{
                color: log.includes("cleanup") ? "#f472b6" : log.includes("Effect") ? "#61afef" : "#98c379",
                marginBottom: 4,
              }}
            >
              {"> "}{log}
            </div>
          ))}
        </div>
      </div>
    </div>
  )
}

// ==================== シーン1: 基本的なuseEffect ====================

const Scene1_BasicEffect: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `function Counter() {
  const [count, setCount] = useState(0)

  useEffect(() => {
    console.log('Effect ran! Count:', count)
  })

  return (
    <button onClick={() => setCount(c => c + 1)}>
      {count}
    </button>
  )
}`

  // アニメーションステート（3倍速: 0.67フレーム/文字 = frame * 1.5）
  const typingSpeed = 1.5  // 1フレームで1.5文字
  const codeProgress = Math.min(frame * typingSpeed, code.length)
  const displayCode = code.slice(0, Math.floor(codeProgress))

  // 挙動シミュレーション（コード完了後に動作開始）
  const codeCompleteFrame = Math.ceil(code.length / typingSpeed) + 20
  const mountFrame = codeCompleteFrame
  const click1Frame = codeCompleteFrame + 50
  const click2Frame = codeCompleteFrame + 100

  let count = 0
  let phase: "mount" | "update" | "unmount" | "idle" = "idle"
  let effectRunning = false
  const logs: string[] = []

  if (frame >= mountFrame) {
    phase = "mount"
    if (frame >= mountFrame && frame < mountFrame + 30) {
      effectRunning = true
    }
    logs.push("Effect ran! Count: 0")
  }

  if (frame >= click1Frame) {
    count = 1
    phase = "update"
    if (frame >= click1Frame && frame < click1Frame + 30) {
      effectRunning = true
    }
    logs.push("Effect ran! Count: 1")
  }

  if (frame >= click2Frame) {
    count = 2
    phase = "update"
    if (frame >= click2Frame && frame < click2Frame + 30) {
      effectRunning = true
    }
    logs.push("Effect ran! Count: 2")
  }

  if (frame >= click2Frame + 50) {
    phase = "idle"
    effectRunning = false
  }

  return (
    <TwoColumnLayout
      title="依存配列なし → 毎回実行"
      code={displayCode}
      codeStartFrame={0}
      codeCompleteFrame={codeCompleteFrame}
    >
      <ComponentVisualizer
        count={count}
        logs={logs.slice(-4)}
        phase={phase}
        effectRunning={effectRunning}
      />
    </TwoColumnLayout>
  )
}

// ==================== シーン2: 空の依存配列 ====================

const Scene2_EmptyDeps: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `function Counter() {
  const [count, setCount] = useState(0)

  useEffect(() => {
    console.log('Effect ran! (mount only)')
  }, [])  // 空の配列

  return (
    <button onClick={() => setCount(c => c + 1)}>
      {count}
    </button>
  )
}`

  // 3倍速タイピング
  const typingSpeed = 1.5
  const codeProgress = Math.min(frame * typingSpeed, code.length)
  const displayCode = code.slice(0, Math.floor(codeProgress))

  // コード完了後に動作開始
  const codeCompleteFrame = Math.ceil(code.length / typingSpeed) + 20
  const mountFrame = codeCompleteFrame
  const click1Frame = codeCompleteFrame + 50
  const click2Frame = codeCompleteFrame + 100

  let count = 0
  let phase: "mount" | "update" | "unmount" | "idle" = "idle"
  let effectRunning = false
  const logs: string[] = []

  if (frame >= mountFrame) {
    phase = "mount"
    if (frame >= mountFrame && frame < mountFrame + 30) {
      effectRunning = true
    }
    logs.push("Effect ran! (mount only)")
  }

  if (frame >= click1Frame) {
    count = 1
    phase = "update"
    // Effectは実行されない！
  }

  if (frame >= click2Frame) {
    count = 2
    phase = "update"
  }

  if (frame >= click2Frame + 50) {
    phase = "idle"
    effectRunning = false
  }

  return (
    <TwoColumnLayout
      title="空の依存配列 [] → マウント時のみ"
      code={displayCode}
      codeStartFrame={0}
      highlightLine={5}
      codeCompleteFrame={codeCompleteFrame}
    >
      <ComponentVisualizer
        count={count}
        logs={logs}
        phase={phase}
        effectRunning={effectRunning}
      />
      {frame >= click1Frame && (
        <div
          style={{
            position: "absolute",
            bottom: 20,
            right: 20,
            background: "rgba(34, 197, 94, 0.2)",
            border: "2px solid #22c55e",
            borderRadius: 12,
            padding: "12px 20px",
            color: "#22c55e",
            fontSize: 16,
            fontFamily: "system-ui",
          }}
        >
          ✓ Updateしても Effect は実行されない！
        </div>
      )}
    </TwoColumnLayout>
  )
}

// ==================== シーン3: 依存値あり ====================

const Scene3_WithDeps: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `function Counter() {
  const [count, setCount] = useState(0)
  const [name, setName] = useState('React')

  useEffect(() => {
    document.title = \`Count: \${count}\`
    console.log('Title updated:', count)
  }, [count])  // countが変わった時だけ

  return <div>...</div>
}`

  // 3倍速タイピング
  const typingSpeed = 1.5
  const codeProgress = Math.min(frame * typingSpeed, code.length)
  const displayCode = code.slice(0, Math.floor(codeProgress))

  // コード完了後に動作開始
  const codeCompleteFrame = Math.ceil(code.length / typingSpeed) + 20
  const mountFrame = codeCompleteFrame
  const countChangeFrame = codeCompleteFrame + 60
  const nameChangeFrame = codeCompleteFrame + 120

  let count = 0
  let phase: "mount" | "update" | "unmount" | "idle" = "idle"
  let effectRunning = false
  const logs: string[] = []
  let docTitle = "React App"

  if (frame >= mountFrame) {
    phase = "mount"
    if (frame >= mountFrame && frame < mountFrame + 30) {
      effectRunning = true
    }
    logs.push("Title updated: 0")
    docTitle = "Count: 0"
  }

  if (frame >= countChangeFrame) {
    count = 1
    phase = "update"
    if (frame >= countChangeFrame && frame < countChangeFrame + 30) {
      effectRunning = true
    }
    logs.push("Title updated: 1")
    docTitle = "Count: 1"
  }

  if (frame >= nameChangeFrame) {
    phase = "update"
    // nameが変わってもcountは変わらないのでEffectは実行されない
    effectRunning = false
  }

  return (
    <TwoColumnLayout
      title="依存値 [count] → countが変わった時のみ"
      code={displayCode}
      codeStartFrame={0}
      highlightLine={7}
      codeCompleteFrame={codeCompleteFrame}
    >
      <div style={{ display: "flex", flexDirection: "column", gap: 24, height: "100%" }}>
        {/* ブラウザタブ表示 */}
        <div
          style={{
            background: "#374151",
            borderRadius: 8,
            padding: 12,
            display: "flex",
            alignItems: "center",
            gap: 12,
          }}
        >
          <div
            style={{
              background: "#1f2937",
              borderRadius: 6,
              padding: "8px 16px",
              color: "white",
              fontSize: 14,
              fontFamily: "system-ui",
            }}
          >
            📄 {docTitle}
          </div>
        </div>

        <ComponentVisualizer
          count={count}
          logs={logs.slice(-3)}
          phase={phase}
          effectRunning={effectRunning}
        />

        {frame >= nameChangeFrame && (
          <div
            style={{
              background: "rgba(251, 191, 36, 0.2)",
              border: "2px solid #fbbf24",
              borderRadius: 12,
              padding: "12px 20px",
              color: "#fbbf24",
              fontSize: 16,
              fontFamily: "system-ui",
            }}
          >
            💡 nameが変わっても、countは変わらないのでEffectは実行されない
          </div>
        )}
      </div>
    </TwoColumnLayout>
  )
}

// ==================== シーン4: クリーンアップ ====================

const Scene4_Cleanup: React.FC = () => {
  const frame = useCurrentFrame()

  const code = `function Timer() {
  const [seconds, setSeconds] = useState(0)

  useEffect(() => {
    const id = setInterval(() => {
      setSeconds(s => s + 1)
    }, 1000)

    // クリーンアップ関数
    return () => {
      clearInterval(id)
      console.log('Timer cleaned up!')
    }
  }, [])

  return <div>{seconds}s</div>
}`

  // 3倍速タイピング
  const typingSpeed = 1.5
  const codeProgress = Math.min(frame * typingSpeed, code.length)
  const displayCode = code.slice(0, Math.floor(codeProgress))

  // コード完了後に動作開始
  const codeCompleteFrame = Math.ceil(code.length / typingSpeed) + 20
  const mountFrame = codeCompleteFrame
  const tick1 = codeCompleteFrame + 40
  const tick2 = codeCompleteFrame + 80
  const tick3 = codeCompleteFrame + 120
  const unmountFrame = codeCompleteFrame + 160

  let seconds = 0
  let phase: "mount" | "update" | "unmount" | "idle" = "idle"
  let effectRunning = false
  const logs: string[] = []

  if (frame >= mountFrame) {
    phase = "mount"
    if (frame < mountFrame + 30) effectRunning = true
    logs.push("Timer started")
  }

  if (frame >= tick1) { seconds = 1; phase = "update" }
  if (frame >= tick2) { seconds = 2 }
  if (frame >= tick3) { seconds = 3 }

  if (frame >= unmountFrame) {
    phase = "unmount"
    effectRunning = true
    logs.push("Timer cleaned up!")
  }

  return (
    <TwoColumnLayout
      title="クリーンアップ関数 → Unmount時に実行"
      code={displayCode}
      codeStartFrame={0}
      highlightLine={9}
      codeCompleteFrame={codeCompleteFrame}
    >
      <div style={{ display: "flex", flexDirection: "column", gap: 24, height: "100%" }}>
        {/* タイマー表示 */}
        {phase !== "unmount" ? (
          <div
            style={{
              background: "#1f2937",
              borderRadius: 16,
              padding: 40,
              display: "flex",
              flexDirection: "column",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <div
              style={{
                fontSize: 80,
                fontWeight: 700,
                color: "#61afef",
                fontFamily: "'Fira Code', monospace",
              }}
            >
              {seconds}s
            </div>
            <div
              style={{
                marginTop: 16,
                color: "#9ca3af",
                fontSize: 18,
                fontFamily: "system-ui",
              }}
            >
              ⏱️ Timer Running
            </div>
          </div>
        ) : (
          <div
            style={{
              background: "#1f2937",
              borderRadius: 16,
              padding: 40,
              display: "flex",
              flexDirection: "column",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <div style={{ fontSize: 64, marginBottom: 16 }}>🧹</div>
            <div
              style={{
                color: "#f472b6",
                fontSize: 24,
                fontFamily: "system-ui",
                fontWeight: 600,
              }}
            >
              Cleanup Executed!
            </div>
            <div
              style={{
                marginTop: 12,
                color: "#9ca3af",
                fontSize: 16,
                fontFamily: "system-ui",
              }}
            >
              clearInterval(id) が呼ばれた
            </div>
          </div>
        )}

        {/* ログパネル */}
        <div
          style={{
            background: "#1e1e1e",
            borderRadius: 12,
            padding: 16,
            fontFamily: "'Fira Code', monospace",
            fontSize: 14,
          }}
        >
          <div style={{ color: "#6b7280", marginBottom: 8 }}>Console:</div>
          {logs.map((log, i) => (
            <div
              key={i}
              style={{
                color: log.includes("cleaned") ? "#f472b6" : "#98c379",
                marginBottom: 4,
              }}
            >
              {"> "}{log}
            </div>
          ))}
        </div>

        {/* フェーズ表示 */}
        <div
          style={{
            display: "flex",
            gap: 12,
          }}
        >
          {["mount", "update", "unmount"].map((p) => (
            <div
              key={p}
              style={{
                padding: "8px 16px",
                borderRadius: 8,
                background: phase === p ? (p === "unmount" ? "#ef4444" : p === "mount" ? "#22c55e" : "#3b82f6") : "rgba(255,255,255,0.1)",
                color: phase === p ? "white" : "#6b7280",
                fontSize: 14,
                fontFamily: "system-ui",
                fontWeight: 600,
              }}
            >
              {p}
            </div>
          ))}
        </div>
      </div>
    </TwoColumnLayout>
  )
}

// ==================== 2カラムレイアウト ====================

const TwoColumnLayout: React.FC<{
  title: string
  code: string
  codeStartFrame: number
  highlightLine?: number
  codeCompleteFrame?: number  // コード完了フレーム
  children: React.ReactNode
}> = ({ title, code, codeStartFrame, highlightLine, codeCompleteFrame = 0, children }) => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const titleOpacity = interpolate(frame, [0, 20], [0, 1], { extrapolateRight: "clamp" })

  // コード完了後のスムーズな表示切り替え
  const showBehavior = codeCompleteFrame > 0 ? frame >= codeCompleteFrame - 10 : true
  const behaviorOpacity = codeCompleteFrame > 0
    ? interpolate(frame, [codeCompleteFrame - 10, codeCompleteFrame + 20], [0.3, 1], {
        extrapolateLeft: "clamp",
        extrapolateRight: "clamp",
      })
    : 1
  const behaviorScale = codeCompleteFrame > 0
    ? spring({
        frame: Math.max(0, frame - codeCompleteFrame + 10),
        fps,
        config: { damping: 15, stiffness: 100 },
      }) * 0.05 + 0.95
    : 1

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #0f172a 0%, #1e293b 100%)",
        padding: 40,
      }}
    >
      {/* タイトル */}
      <div
        style={{
          opacity: titleOpacity,
          marginBottom: 24,
        }}
      >
        <h2
          style={{
            fontSize: 36,
            color: "#61afef",
            fontFamily: "system-ui",
            margin: 0,
          }}
        >
          {title}
        </h2>
      </div>

      {/* 2カラム */}
      <div
        style={{
          display: "flex",
          gap: 40,
          flex: 1,
        }}
      >
        {/* 左: コード */}
        <div
          style={{
            flex: 1,
            background: "#1e1e1e",
            borderRadius: 16,
            padding: 24,
            overflow: "hidden",
          }}
        >
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 8,
              marginBottom: 16,
              paddingBottom: 12,
              borderBottom: "1px solid rgba(255,255,255,0.1)",
            }}
          >
            <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#ef4444" }} />
            <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#fbbf24" }} />
            <div style={{ width: 12, height: 12, borderRadius: "50%", background: "#22c55e" }} />
            <span style={{ marginLeft: 12, color: "#6b7280", fontSize: 14 }}>App.tsx</span>
          </div>

          <HighlightedCode code={code} />
        </div>

        {/* 右: 挙動（コード完了後にスムーズ表示） */}
        <div
          style={{
            flex: 1,
            position: "relative",
            opacity: behaviorOpacity,
            transform: `scale(${behaviorScale})`,
            transition: "transform 0.3s ease-out",
          }}
        >
          {children}
        </div>
      </div>
    </AbsoluteFill>
  )
}

// ==================== まとめスライド ====================

const SummarySlide: React.FC<{
  title: string
  points: string[]
  icon: string
  color: string
  nextHint?: string
}> = ({ title, points, icon, color, nextHint }) => {
  const frame = useCurrentFrame()
  const { fps, durationInFrames } = useVideoConfig()

  const scale = spring({ frame, fps, config: { damping: 12 } })

  // フェードアウト
  const fadeOut = interpolate(
    frame,
    [durationInFrames - 20, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  )

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)",
        justifyContent: "center",
        alignItems: "center",
        opacity: fadeOut,
      }}
    >
      <div
        style={{
          transform: `scale(${Math.min(scale, 1)})`,
          textAlign: "center",
          maxWidth: 900,
        }}
      >
        {/* アイコン */}
        <div style={{ fontSize: 80, marginBottom: 24 }}>{icon}</div>

        {/* タイトル */}
        <h2
          style={{
            fontSize: 48,
            color,
            fontFamily: "system-ui",
            marginBottom: 40,
          }}
        >
          {title}
        </h2>

        {/* ポイント */}
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
          {points.map((point, i) => {
            const pointDelay = 20 + i * 15
            const pointOpacity = interpolate(frame - pointDelay, [0, 15], [0, 1], {
              extrapolateLeft: "clamp",
              extrapolateRight: "clamp",
            })
            const pointY = interpolate(frame - pointDelay, [0, 15], [20, 0], {
              extrapolateLeft: "clamp",
              extrapolateRight: "clamp",
            })

            return (
              <div
                key={i}
                style={{
                  opacity: pointOpacity,
                  transform: `translateY(${pointY}px)`,
                  background: "rgba(255,255,255,0.05)",
                  padding: "16px 32px",
                  borderRadius: 12,
                  borderLeft: `4px solid ${color}`,
                }}
              >
                <p
                  style={{
                    color: "white",
                    fontSize: 28,
                    margin: 0,
                    fontFamily: "system-ui",
                    textAlign: "left",
                  }}
                >
                  {point}
                </p>
              </div>
            )
          })}
        </div>

        {/* 次のヒント */}
        {nextHint && (
          <div
            style={{
              marginTop: 50,
              opacity: interpolate(frame, [60, 75], [0, 1], {
                extrapolateLeft: "clamp",
                extrapolateRight: "clamp",
              }),
            }}
          >
            <p
              style={{
                color: "#6b7280",
                fontSize: 20,
                fontFamily: "system-ui",
              }}
            >
              次: {nextHint} →
            </p>
          </div>
        )}
      </div>
    </AbsoluteFill>
  )
}

// まとめ1: 依存配列なし
const Summary1: React.FC = () => (
  <SummarySlide
    icon="🔄"
    title="依存配列なし = 毎回実行"
    color="#ef4444"
    points={[
      "useEffect(() => { ... }) ← 配列なし",
      "コンポーネントがレンダリングされるたびに実行",
      "パフォーマンスに注意が必要",
    ]}
    nextHint="空の依存配列 []"
  />
)

// まとめ2: 空の依存配列
const Summary2: React.FC = () => (
  <SummarySlide
    icon="1️⃣"
    title="空配列 [] = マウント時のみ"
    color="#22c55e"
    points={[
      "useEffect(() => { ... }, []) ← 空配列",
      "コンポーネントの初回マウント時のみ実行",
      "初期データ取得やイベント登録に最適",
    ]}
    nextHint="依存値を指定"
  />
)

// まとめ3: 依存値あり
const Summary3: React.FC = () => (
  <SummarySlide
    icon="👀"
    title="[deps] = 依存値の変更時"
    color="#3b82f6"
    points={[
      "useEffect(() => { ... }, [count]) ← 依存値",
      "指定した値が変わった時だけ実行",
      "関係ない値の変更では実行されない",
    ]}
    nextHint="クリーンアップ関数"
  />
)

// まとめ4: クリーンアップ
const Summary4: React.FC = () => (
  <SummarySlide
    icon="🧹"
    title="return () => { } = クリーンアップ"
    color="#f472b6"
    points={[
      "Unmount時やEffect再実行前に呼ばれる",
      "タイマー、イベントリスナー、購読を解除",
      "メモリリークを防ぐ重要な仕組み",
    ]}
  />
)

// ==================== メインコンポーネント ====================

// シーン間のトランジション（2秒の間隔）
const SceneTransition: React.FC = () => {
  const frame = useCurrentFrame()
  const { fps } = useVideoConfig()

  const opacity = interpolate(frame, [0, 30, 30, 60], [0, 0.5, 0.5, 0], {
    extrapolateRight: "clamp",
  })

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #0f172a 0%, #1e293b 100%)",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      <div
        style={{
          opacity,
          fontSize: 24,
          color: "#6b7280",
          fontFamily: "system-ui",
        }}
      >
        ● ● ●
      </div>
    </AbsoluteFill>
  )
}

export const UseEffectInteractive: React.FC = () => {
  // タイピング高速化により各シーンを短縮
  const scene1 = 280
  const summary1 = 90
  const transition = 60  // 2秒の間隔
  const scene2 = 280
  const summary2 = 90
  const scene3 = 320
  const summary3 = 90
  const scene4 = 350
  const summary4 = 90

  let offset = 0

  return (
    <AbsoluteFill>
      {/* シーン1: 依存配列なし */}
      <Sequence from={offset} durationInFrames={scene1}>
        <Scene1_BasicEffect />
      </Sequence>
      <Sequence from={(offset += scene1)} durationInFrames={summary1}>
        <Summary1 />
      </Sequence>

      {/* 2秒の間隔 */}
      <Sequence from={(offset += summary1)} durationInFrames={transition}>
        <SceneTransition />
      </Sequence>

      {/* シーン2: 空の依存配列 */}
      <Sequence from={(offset += transition)} durationInFrames={scene2}>
        <Scene2_EmptyDeps />
      </Sequence>
      <Sequence from={(offset += scene2)} durationInFrames={summary2}>
        <Summary2 />
      </Sequence>

      {/* 2秒の間隔 */}
      <Sequence from={(offset += summary2)} durationInFrames={transition}>
        <SceneTransition />
      </Sequence>

      {/* シーン3: 依存値あり */}
      <Sequence from={(offset += transition)} durationInFrames={scene3}>
        <Scene3_WithDeps />
      </Sequence>
      <Sequence from={(offset += scene3)} durationInFrames={summary3}>
        <Summary3 />
      </Sequence>

      {/* 2秒の間隔 */}
      <Sequence from={(offset += summary3)} durationInFrames={transition}>
        <SceneTransition />
      </Sequence>

      {/* シーン4: クリーンアップ */}
      <Sequence from={(offset += transition)} durationInFrames={scene4}>
        <Scene4_Cleanup />
      </Sequence>
      <Sequence from={(offset += scene4)} durationInFrames={summary4}>
        <Summary4 />
      </Sequence>
    </AbsoluteFill>
  )
}
