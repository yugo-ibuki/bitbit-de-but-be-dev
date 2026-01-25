import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  useVideoConfig,
  spring,
} from "remotion";

type HelloWorldProps = {
  titleText: string;
  titleColor: string;
};

export const HelloWorld: React.FC<HelloWorldProps> = ({
  titleText,
  titleColor,
}) => {
  const frame = useCurrentFrame();
  const { fps, durationInFrames } = useVideoConfig();

  // フェードインアニメーション
  const opacity = interpolate(frame, [0, 30], [0, 1], {
    extrapolateRight: "clamp",
  });

  // スプリングアニメーションでスケール
  const scale = spring({
    frame,
    fps,
    config: {
      damping: 200,
    },
  });

  // テキストのY位置アニメーション
  const translateY = interpolate(frame, [0, 30], [50, 0], {
    extrapolateRight: "clamp",
  });

  // 背景のグラデーション回転
  const rotate = interpolate(frame, [0, durationInFrames], [0, 360]);

  return (
    <AbsoluteFill
      style={{
        background: `linear-gradient(${rotate}deg, #667eea 0%, #764ba2 100%)`,
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      <div
        style={{
          opacity,
          transform: `scale(${scale}) translateY(${translateY}px)`,
          fontSize: 100,
          fontWeight: "bold",
          color: titleColor,
          textShadow: "4px 4px 8px rgba(0, 0, 0, 0.3)",
          fontFamily: "sans-serif",
        }}
      >
        {titleText}
      </div>

      {/* フレームカウンター（デバッグ用） */}
      <div
        style={{
          position: "absolute",
          bottom: 50,
          right: 50,
          fontSize: 24,
          color: "rgba(255, 255, 255, 0.7)",
          fontFamily: "monospace",
        }}
      >
        Frame: {frame} / {durationInFrames}
      </div>
    </AbsoluteFill>
  );
};
