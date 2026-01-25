import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  spring,
  useVideoConfig,
} from "remotion";

export const OutroScene: React.FC = () => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames } = useVideoConfig();

  // メインテキストのアニメーション
  const textScale = spring({
    frame,
    fps,
    config: { damping: 12, stiffness: 100 },
  });

  const textOpacity = interpolate(frame, [0, 20], [0, 1], {
    extrapolateRight: "clamp",
  });

  // CTAボタンのアニメーション
  const ctaDelay = 25;
  const ctaScale = spring({
    frame: frame - ctaDelay,
    fps,
    config: { damping: 15 },
  });

  const ctaOpacity = interpolate(frame, [ctaDelay, ctaDelay + 15], [0, 1], {
    extrapolateLeft: "clamp",
    extrapolateRight: "clamp",
  });

  // ボタンのパルスアニメーション
  const pulseFrame = Math.max(0, frame - ctaDelay - 20);
  const pulse = 1 + Math.sin(pulseFrame * 0.15) * 0.03;

  // 背景のパーティクル
  const stars = Array.from({ length: 50 }, (_, i) => {
    const x = (i * 137.5) % 100;
    const y = (i * 73.7) % 100;
    const size = 2 + (i % 3);
    const twinkle = Math.sin(frame * 0.1 + i) * 0.5 + 0.5;

    return { x, y, size, opacity: twinkle * 0.8 };
  });

  // フェードアウト
  const fadeOut = interpolate(
    frame,
    [durationInFrames - 30, durationInFrames],
    [1, 0],
    { extrapolateLeft: "clamp", extrapolateRight: "clamp" }
  );

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(180deg, #0f0c29 0%, #302b63 50%, #24243e 100%)",
        justifyContent: "center",
        alignItems: "center",
        opacity: fadeOut,
      }}
    >
      {/* 背景の星 */}
      {stars.map((star, i) => (
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
            opacity: star.opacity,
          }}
        />
      ))}

      {/* グロー効果 */}
      <div
        style={{
          position: "absolute",
          width: 600,
          height: 600,
          borderRadius: "50%",
          background: "radial-gradient(circle, rgba(99, 102, 241, 0.3) 0%, transparent 70%)",
          filter: "blur(40px)",
        }}
      />

      <div style={{ textAlign: "center", zIndex: 1 }}>
        {/* メインテキスト */}
        <h1
          style={{
            fontSize: 72,
            fontWeight: 800,
            color: "white",
            margin: 0,
            marginBottom: 24,
            fontFamily: "system-ui, sans-serif",
            opacity: textOpacity,
            transform: `scale(${textScale})`,
            textShadow: "0 4px 30px rgba(99, 102, 241, 0.5)",
          }}
        >
          Start Creating
        </h1>

        <p
          style={{
            fontSize: 28,
            color: "rgba(255, 255, 255, 0.7)",
            margin: 0,
            marginBottom: 50,
            fontFamily: "system-ui, sans-serif",
            opacity: textOpacity,
          }}
        >
          Build stunning videos with code
        </p>

        {/* CTAボタン */}
        <div
          style={{
            opacity: ctaOpacity,
            transform: `scale(${ctaScale * pulse})`,
          }}
        >
          <div
            style={{
              display: "inline-flex",
              alignItems: "center",
              gap: 12,
              background: "linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%)",
              padding: "20px 48px",
              borderRadius: 50,
              boxShadow: "0 10px 40px rgba(99, 102, 241, 0.5)",
            }}
          >
            <span
              style={{
                fontSize: 24,
                fontWeight: 600,
                color: "white",
                fontFamily: "system-ui, sans-serif",
              }}
            >
              npm init video
            </span>
            <span style={{ fontSize: 24 }}>→</span>
          </div>
        </div>

        {/* URL */}
        <p
          style={{
            fontSize: 20,
            color: "rgba(255, 255, 255, 0.5)",
            marginTop: 40,
            fontFamily: "system-ui, sans-serif",
            opacity: ctaOpacity,
          }}
        >
          remotion.dev
        </p>
      </div>
    </AbsoluteFill>
  );
};
