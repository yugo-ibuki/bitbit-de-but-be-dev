# mcp-mock

Codex App の Browser Use と MCP Inspector の相性を記事で見せるための、軽い Hono 製 MCP モックサーバです。

## 起動

```bash
npm install
npm run dev
```

ブラウザ確認用:

```text
http://localhost:8787
```

MCP Inspector では Transport に `Streamable HTTP` を選び、URL に以下を入れます。

```text
http://localhost:8787/mcp
```

Inspector 自体は別ターミナルで起動できます。

```bash
npx @modelcontextprotocol/inspector
```

## 中身

- Tool: `summarize_demo_context`
- Tool: `score_article_workflow`
- Resource: `mock://article/context`
- Prompt: `draft_article_outline`

## 確認

```bash
npm test
npm run build
```
