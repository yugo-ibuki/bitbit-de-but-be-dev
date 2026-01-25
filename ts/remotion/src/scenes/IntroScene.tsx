import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  spring,
  useVideoConfig,
} from "remotion";

export const IntroScene: React.FC = () => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();

  // ロゴのスプリングアニメーション
  const logoScale = spring({
    frame,
    fps,
    config: { damping: 12, stiffness: 100 },
  });

  // 円が広がるアニメーション
  const circleScale = spring({
    frame: frame - 10,
    fps,
    config: { damping: 20 },
  });

  // タイトルのフェードイン
  const titleOpacity = interpolate(frame, [20, 40], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  const titleY = interpolate(frame, [20, 40], [30, 0], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  // サブタイトル
  const subtitleOpacity = interpolate(frame, [35, 50], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  // パーティクル生成
  const particles = Array.from({ length: 20 }, (_, i) => {
    const angle = (i / 20) * Math.PI * 2;
    const delay = i * 2;
    const particleFrame = Math.max(0, frame - delay - 15);
    const distance = interpolate(particleFrame, [0, 30], [0, 300], {
      extrapolateRight: "clamp",
    });
    const particleOpacity = interpolate(particleFrame, [0, 20, 40], [0, 1, 0], {
      extrapolateRight: "clamp",
    });

    return {
      x: Math.cos(angle) * distance,
      y: Math.sin(angle) * distance,
      opacity: particleOpacity,
      size: 8 + (i % 3) * 4,
      color: i % 2 === 0 ? "#60a5fa" : "#a78bfa",
    };
  });

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1e1b4b 0%, #312e81 50%, #1e1b4b 100%)",
        justifyContent: "center",
        alignItems: "center",
        overflow: "hidden",
      }}
    >
      {/* 背景の円 */}
      <div
        style={{
          position: "absolute",
          width: 800,
          height: 800,
          borderRadius: "50%",
          background: "radial-gradient(circle, rgba(99, 102, 241, 0.3) 0%, transparent 70%)",
          transform: `scale(${circleScale})`,
        }}
      />

      {/* パーティクル */}
      {particles.map((p, i) => (
        <div
          key={i}
          style={{
            position: "absolute",
            width: p.size,
            height: p.size,
            borderRadius: "50%",
            backgroundColor: p.color,
            transform: `translate(${p.x}px, ${p.y}px)`,
            opacity: p.opacity,
            boxShadow: `0 0 ${p.size}px ${p.color}`,
          }}
        />
      ))}

      {/* ロゴ/アイコン */}
      <div
        style={{
          transform: `scale(${logoScale})`,
          marginBottom: 40,
        }}
      >
        <div
          style={{
            width: 120,
            height: 120,
            borderRadius: 30,
            background: "linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%)",
            display: "flex",
            justifyContent: "center",
            alignItems: "center",
            boxShadow: "0 20px 60px rgba(99, 102, 241, 0.5)",
          }}
        >
          <span style={{ fontSize: 60, color: "white" }}>▶</span>
        </div>
      </div>

      {/* タイトル */}
      <div
        style={{
          opacity: titleOpacity,
          transform: `translateY(${titleY}px)`,
          textAlign: "center",
        }}
      >
        <h1
          style={{
            fontSize: 80,
            fontWeight: 800,
            color: "white",
            margin: 0,
            fontFamily: "system-ui, sans-serif",
            letterSpacing: -2,
          }}
        >
          Remotion Demo
        </h1>
      </div>

      {/* サブタイトル */}
      <div
        style={{
          opacity: subtitleOpacity,
          marginTop: 20,
        }}
      >
        <p
          style={{
            fontSize: 28,
            color: "rgba(255, 255, 255, 0.7)",
            margin: 0,
            fontFamily: "system-ui, sans-serif",
            fontWeight: 300,
          }}
        >
          Programmatic Video with React
        </p>
      </div>
    </AbsoluteFill>
  );
};
