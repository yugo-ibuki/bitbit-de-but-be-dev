import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import {
  calculateInspectorScore,
  demoContext,
  makeDemoSummary,
} from "./demo-data.js";

const articlePrompts = [
  {
    name: "draft_article_outline",
    title: "Draft Article Outline",
    description:
      "Create a compact outline for an article about Codex App Browser Use and MCP Inspector.",
    text: {
      casual:
        "Codex App Browser Use と MCP Inspector の相性の良さが伝わる、軽めの記事構成案を作ってください。",
      technical:
        "Codex App Browser Use と MCP Inspector を組み合わせて MCP サーバを検証する技術記事の構成案を作ってください。",
    },
  },
  {
    name: "write_intro_hook",
    title: "Write Intro Hook",
    description: "Write the opening paragraph for the article.",
    text: {
      casual:
        "MCP Inspector と Codex App Browser Use を同時に使うと何がうれしいのか、読者を引き込む導入文を書いてください。",
      technical:
        "MCP Inspector と Codex App Browser Use を組み合わせた検証フローの利点を、技術読者向けの導入文として書いてください。",
    },
  },
  {
    name: "explain_mcp_endpoint",
    title: "Explain MCP Endpoint",
    description: "Explain what the /mcp endpoint is doing in this mock server.",
    text: {
      casual:
        "このモックサーバの /mcp エンドポイントが何をしているのか、MCP 初学者向けにやさしく説明してください。",
      technical:
        "Hono 上の /mcp エンドポイントが Streamable HTTP transport として振る舞う流れを技術的に説明してください。",
    },
  },
  {
    name: "compare_browser_and_inspector",
    title: "Compare Browser And Inspector",
    description: "Compare the roles of Browser Use and MCP Inspector.",
    text: {
      casual:
        "Browser Use と MCP Inspector の役割の違いを、記事用に短く比較してください。",
      technical:
        "Browser Use による UI 検証と MCP Inspector によるプロトコル検証の責務分担を比較してください。",
    },
  },
  {
    name: "make_screenshot_checklist",
    title: "Make Screenshot Checklist",
    description: "Create a checklist for article screenshots.",
    text: {
      casual:
        "記事に載せるスクリーンショットを撮る前の確認チェックリストを作ってください。",
      technical:
        "MCP Inspector と Browser Use の検証画面を記事に載せる前の技術的チェックリストを作ってください。",
    },
  },
  {
    name: "summarize_tools_tab",
    title: "Summarize Tools Tab",
    description: "Summarize what readers should notice in the Inspector Tools tab.",
    text: {
      casual:
        "MCP Inspector の Tools タブで読者に見てほしいポイントを短くまとめてください。",
      technical:
        "MCP Inspector の Tools タブで tools/list と tools/call の確認観点を説明してください。",
    },
  },
  {
    name: "summarize_prompts_tab",
    title: "Summarize Prompts Tab",
    description: "Summarize what readers should notice in the Inspector Prompts tab.",
    text: {
      casual:
        "MCP Inspector の Prompts タブで prompt を確認する流れを、記事向けに説明してください。",
      technical:
        "MCP Inspector の Prompts タブで prompts/list と prompts/get を確認する観点を説明してください。",
    },
  },
  {
    name: "write_failure_note",
    title: "Write Failure Note",
    description: "Write a short note for common connection failures.",
    text: {
      casual:
        "Inspector が接続できないときに読者が確認すべきことを、短い注意書きとして書いてください。",
      technical:
        "Streamable HTTP の接続で失敗したときの確認項目を、ステータスコードや URL 設定の観点でまとめてください。",
    },
  },
  {
    name: "draft_conclusion",
    title: "Draft Conclusion",
    description: "Draft the article conclusion.",
    text: {
      casual:
        "Codex App Browser Use と MCP Inspector を一緒に使う価値が伝わる締めの文章を書いてください。",
      technical:
        "UI と MCP プロトコルの両面を検証できる開発フローとして、記事の結論を書いてください。",
    },
  },
  {
    name: "make_social_post",
    title: "Make Social Post",
    description: "Create a short social post announcing the article.",
    text: {
      casual:
        "この記事を公開したときの SNS 投稿文を、カジュアルに 120 字以内で書いてください。",
      technical:
        "MCP Inspector と Codex App Browser Use の検証フローを紹介する SNS 投稿文を 120 字以内で書いてください。",
    },
  },
] as const;

export const articlePromptNames = articlePrompts.map((prompt) => prompt.name);

export function createMcpServer(): McpServer {
  const server = new McpServer({
    name: "codex-browser-mcp-mock",
    version: "0.1.0",
  });

  server.registerTool(
    "summarize_demo_context",
    {
      title: "Summarize Demo Context",
      description:
        "Return short Japanese-article-friendly notes about the browser and MCP Inspector demo.",
      inputSchema: {
        target: z.enum(["browser", "inspector", "all"]).default("all"),
      },
      outputSchema: {
        summary: z.string(),
      },
    },
    async ({ target }) => {
      const output = { summary: makeDemoSummary(target) };
      return {
        content: [{ type: "text", text: output.summary }],
        structuredContent: output,
      };
    },
  );

  server.registerTool(
    "score_article_workflow",
    {
      title: "Score Article Workflow",
      description:
        "Score whether the mock demo has enough visible signals for an article screenshot flow.",
      inputSchema: {
        browserChecked: z.boolean(),
        inspectorConnected: z.boolean(),
        toolsCalled: z.number().int().min(0).default(0),
      },
      outputSchema: {
        score: z.number(),
        label: z.enum(["draft", "almost-there", "article-ready"]),
      },
    },
    async (input) => {
      const output = calculateInspectorScore(input);
      return {
        content: [
          {
            type: "text",
            text: `Score: ${output.score} (${output.label})`,
          },
        ],
        structuredContent: output,
      };
    },
  );

  server.registerResource(
    "article-context",
    "mock://article/context",
    {
      title: "Article Context",
      description: "Mock context for the Codex App Browser Use and MCP Inspector article.",
      mimeType: "application/json",
    },
    async (uri) => ({
      contents: [
        {
          uri: uri.href,
          mimeType: "application/json",
          text: JSON.stringify(demoContext, null, 2),
        },
      ],
    }),
  );

  for (const prompt of articlePrompts) {
    server.registerPrompt(
      prompt.name,
      {
        title: prompt.title,
        description: prompt.description,
        argsSchema: {
          tone: z.enum(["casual", "technical"]).default("casual"),
        },
      },
      ({ tone }) => ({
        messages: [
          {
            role: "user",
            content: {
              type: "text",
              text: prompt.text[tone],
            },
          },
        ],
      }),
    );
  }

  return server;
}
