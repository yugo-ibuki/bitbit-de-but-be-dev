import {
  AbsoluteFill,
  interpolate,
  useCurrentFrame,
  spring,
  useVideoConfig,
} from "remotion";

type TokenType = "keyword" | "string" | "function" | "bracket" | "variable" | "text";

interface Token {
  text: string;
  type: TokenType;
}

const tokenColors: Record<TokenType, string> = {
  keyword: "#c792ea",
  string: "#c3e88d",
  function: "#82aaff",
  bracket: "#89ddff",
  variable: "#f78c6c",
  text: "#a6accd",
};

// シンプルなトークナイザー
const tokenize = (text: string): Token[] => {
  const tokens: Token[] = [];
  const patterns: [RegExp, TokenType][] = [
    [/^(import|from|export|const|return)\b/, "keyword"],
    [/^'[^']*'/, "string"],
    [/^(useCurrentFrame|MyVideo)\b/, "function"],
    [/^[{}()]/, "bracket"],
    [/^(frame|opacity)\b/, "variable"],
    [/^\S+/, "text"],
    [/^\s+/, "text"],
  ];

  let remaining = text;
  while (remaining.length > 0) {
    let matched = false;
    for (const [pattern, type] of patterns) {
      const match = remaining.match(pattern);
      if (match) {
        tokens.push({ text: match[0], type });
        remaining = remaining.slice(match[0].length);
        matched = true;
        break;
      }
    }
    if (!matched) {
      tokens.push({ text: remaining[0], type: "text" });
      remaining = remaining.slice(1);
    }
  }
  return tokens;
};

const codeLines = [
  { indent: 0, text: "import { useCurrentFrame } from 'remotion';" },
  { indent: 0, text: "" },
  { indent: 0, text: "export const MyVideo = () => {" },
  { indent: 1, text: "const frame = useCurrentFrame();" },
  { indent: 1, text: "const opacity = frame / 30;" },
  { indent: 1, text: "" },
  { indent: 1, text: "return (" },
  { indent: 2, text: "<div style={{ opacity }}>" },
  { indent: 3, text: "Hello, World!" },
  { indent: 2, text: "</div>" },
  { indent: 1, text: ");" },
  { indent: 0, text: "};" },
];

const CodeLine: React.FC<{
  line: { indent: number; text: string };
  index: number;
}> = ({ line, index }) => {
  const frame = useCurrentFrame();

  const delay = index * 4;
  const lineFrame = Math.max(0, frame - delay);

  // タイピングアニメーション
  const progress = interpolate(lineFrame, [0, 15], [0, 1], {
    extrapolateRight: "clamp",
  });

  const visibleChars = Math.floor(progress * line.text.length);
  const displayText = line.text.slice(0, visibleChars);

  // カーソルの点滅
  const showCursor =
    lineFrame > 0 && lineFrame < 20 && Math.floor(lineFrame / 4) % 2 === 0;

  // 行のフェードイン
  const opacity = interpolate(lineFrame, [0, 5], [0, 1], {
    extrapolateRight: "clamp",
  });

  const tokens = tokenize(displayText);

  return (
    <div
      style={{
        fontFamily: "'Fira Code', 'SF Mono', monospace",
        fontSize: 24,
        lineHeight: 1.8,
        color: "#a6accd",
        opacity,
        paddingLeft: line.indent * 32,
        whiteSpace: "pre",
      }}
    >
      {tokens.map((token, i) => (
        <span key={i} style={{ color: tokenColors[token.type] }}>
          {token.text}
        </span>
      ))}
      {showCursor && (
        <span
          style={{
            backgroundColor: "#ffcb6b",
            width: 3,
            height: 28,
            display: "inline-block",
            marginLeft: 2,
          }}
        />
      )}
    </div>
  );
};

export const CodeScene: React.FC = () => {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();

  // ウィンドウのスプリングアニメーション
  const windowScale = spring({
    frame,
    fps,
    config: { damping: 15, stiffness: 80 },
  });

  return (
    <AbsoluteFill
      style={{
        background: "linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      {/* コードエディタウィンドウ */}
      <div
        style={{
          transform: `scale(${windowScale})`,
          background: "#1e1e3f",
          borderRadius: 16,
          overflow: "hidden",
          boxShadow: "0 40px 80px rgba(0, 0, 0, 0.5)",
          width: 900,
        }}
      >
        {/* タイトルバー */}
        <div
          style={{
            background: "#2d2d5a",
            padding: "16px 20px",
            display: "flex",
            alignItems: "center",
            gap: 8,
          }}
        >
          <div
            style={{
              width: 14,
              height: 14,
              borderRadius: "50%",
              backgroundColor: "#ff5f57",
            }}
          />
          <div
            style={{
              width: 14,
              height: 14,
              borderRadius: "50%",
              backgroundColor: "#ffbd2e",
            }}
          />
          <div
            style={{
              width: 14,
              height: 14,
              borderRadius: "50%",
              backgroundColor: "#28ca42",
            }}
          />
          <span
            style={{
              marginLeft: 16,
              color: "rgba(255, 255, 255, 0.5)",
              fontSize: 14,
              fontFamily: "system-ui, sans-serif",
            }}
          >
            MyVideo.tsx
          </span>
        </div>

        {/* コード部分 */}
        <div style={{ padding: "24px 32px" }}>
          {codeLines.map((line, i) => (
            <CodeLine key={i} line={line} index={i} />
          ))}
        </div>
      </div>
    </AbsoluteFill>
  );
};
