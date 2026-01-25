import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  spring,
  useVideoConfig,
} from "remotion";

const features = [
  { icon: "⚡", title: "Fast", desc: "60fps rendering" },
  { icon: "🎨", title: "React", desc: "Use your skills" },
  { icon: "🎬", title: "Pro", desc: "Studio quality" },
];

const FeatureCard: React.FC<{
  icon: string;
  title: string;
  desc: string;
  delay: number;
}> = ({ icon, title, desc, delay }) => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();

  const appear = spring({
    frame: frame - delay,
    fps,
    config: { damping: 15, stiffness: 80 },
  });

  const scale = interpolate(appear, [0, 1], [0.5, 1]);
  const opacity = interpolate(appear, [0, 1], [0, 1]);
  const y = interpolate(appear, [0, 1], [50, 0]);

  // ホバーっぽいアニメーション
  const hoverFrame = Math.max(0, frame - delay - 20);
  const floatY = Math.sin(hoverFrame * 0.1) * 5;

  return (
    <div
      style={{
        opacity,
        transform: `scale(${scale}) translateY(${y + floatY}px)`,
        background: "rgba(255, 255, 255, 0.1)",
        backdropFilter: "blur(10px)",
        borderRadius: 24,
        padding: "40px 50px",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        gap: 16,
        border: "1px solid rgba(255, 255, 255, 0.2)",
        boxShadow: "0 20px 40px rgba(0, 0, 0, 0.3)",
      }}
    >
      <span style={{ fontSize: 60 }}>{icon}</span>
      <h3
        style={{
          fontSize: 32,
          fontWeight: 700,
          color: "white",
          margin: 0,
          fontFamily: "system-ui, sans-serif",
        }}
      >
        {title}
      </h3>
      <p
        style={{
          fontSize: 20,
          color: "rgba(255, 255, 255, 0.7)",
          margin: 0,
          fontFamily: "system-ui, sans-serif",
        }}
      >
        {desc}
      </p>
    </div>
  );
};

export const FeatureScene: React.FC = () => {
  const frame = useCurrentFrame();

  // 背景のグラデーション回転
  const gradientAngle = interpolate(frame, [0, 120], [135, 180]);

  // タイトル
  const titleOpacity = interpolate(frame, [0, 20], [0, 1], {
    extrapolateRight: "clamp",
  });
  const titleY = interpolate(frame, [0, 20], [-30, 0], {
    extrapolateRight: "clamp",
  });

  return (
    <AbsoluteFill
      style={{
        background: `linear-gradient(${gradientAngle}deg, #0f172a 0%, #1e293b 50%, #0f172a 100%)`,
        justifyContent: "center",
        alignItems: "center",
        overflow: "hidden",
      }}
    >
      {/* 背景のぼかし円 */}
      <div
        style={{
          position: "absolute",
          width: 600,
          height: 600,
          borderRadius: "50%",
          background: "radial-gradient(circle, rgba(56, 189, 248, 0.2) 0%, transparent 70%)",
          top: -200,
          right: -100,
        }}
      />
      <div
        style={{
          position: "absolute",
          width: 500,
          height: 500,
          borderRadius: "50%",
          background: "radial-gradient(circle, rgba(168, 85, 247, 0.2) 0%, transparent 70%)",
          bottom: -150,
          left: -100,
        }}
      />

      <div style={{ textAlign: "center" }}>
        {/* タイトル */}
        <h2
          style={{
            fontSize: 56,
            fontWeight: 700,
            color: "white",
            marginBottom: 80,
            fontFamily: "system-ui, sans-serif",
            opacity: titleOpacity,
            transform: `translateY(${titleY}px)`,
          }}
        >
          Why Remotion?
        </h2>

        {/* カード */}
        <div
          style={{
            display: "flex",
            gap: 40,
            justifyContent: "center",
          }}
        >
          {features.map((f, i) => (
            <FeatureCard
              key={i}
              icon={f.icon}
              title={f.title}
              desc={f.desc}
              delay={15 + i * 12}
            />
          ))}
        </div>
      </div>
    </AbsoluteFill>
  );
};
