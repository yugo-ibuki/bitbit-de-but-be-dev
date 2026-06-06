export type DemoSignalInput = {
  browserChecked: boolean;
  inspectorConnected: boolean;
  toolsCalled: number;
};

export type DemoScore = {
  score: number;
  label: "draft" | "almost-there" | "article-ready";
};

export const demoContext = {
  title: "Codex App Browser Use x MCP Inspector",
  endpoint: "/mcp",
  transport: "streamable-http",
  tools: ["summarize_demo_context", "score_article_workflow"],
  resources: ["mock://article/context"],
  prompts: ["draft_article_outline"],
} as const;

export function makeDemoSummary(target: "browser" | "inspector" | "all"): string {
  const summaries = {
    browser:
      "Browser Use can open the local mock page, verify visible copy, and capture screenshots for the article.",
    inspector:
      "MCP Inspector can connect to the same Hono server, list tools/resources/prompts, and call demo tools interactively.",
    all:
      "Browser Use checks the user-facing page while MCP Inspector verifies the protocol surface against the /mcp endpoint.",
  };

  return summaries[target];
}

export function calculateInspectorScore(input: DemoSignalInput): DemoScore {
  const browserPoints = input.browserChecked ? 35 : 0;
  const inspectorPoints = input.inspectorConnected ? 40 : 0;
  const toolPoints = Math.min(input.toolsCalled, 2) * 10;
  const score = Math.min(browserPoints + inspectorPoints + toolPoints, 95);

  if (score >= 90) {
    return { score, label: "article-ready" };
  }

  if (score >= 60) {
    return { score, label: "almost-there" };
  }

  return { score, label: "draft" };
}
