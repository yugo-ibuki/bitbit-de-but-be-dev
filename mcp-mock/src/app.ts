import { StreamableHTTPTransport } from "@hono/mcp";
import { Hono } from "hono";
import { cors } from "hono/cors";
import { createMcpServer } from "./mcp-server.js";

const html = String.raw;

export function createApp(): Hono {
  const app = new Hono();

  app.use(
    "/mcp",
    cors({
      origin: "*",
      allowHeaders: ["Content-Type", "Accept", "Mcp-Session-Id"],
      allowMethods: ["GET", "POST", "DELETE", "OPTIONS"],
      exposeHeaders: ["Mcp-Session-Id"],
    }),
  );

  app.get("/", (c) =>
    c.html(html`<!doctype html>
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <title>Codex Browser Use MCP Mock</title>
          <style>
            :root {
              color-scheme: light;
              font-family:
                Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont,
                "Segoe UI", sans-serif;
              background: #f6f7f9;
              color: #1c2430;
            }

            body {
              margin: 0;
            }

            main {
              max-width: 980px;
              margin: 0 auto;
              padding: 56px 24px;
            }

            h1 {
              margin: 0;
              font-size: 42px;
              line-height: 1.1;
              letter-spacing: 0;
            }

            .lead {
              max-width: 720px;
              margin: 18px 0 32px;
              color: #526071;
              font-size: 18px;
              line-height: 1.7;
            }

            .grid {
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
              gap: 16px;
            }

            .panel {
              border: 1px solid #d9dee7;
              border-radius: 8px;
              background: #ffffff;
              padding: 20px;
            }

            .panel h2 {
              margin: 0 0 12px;
              font-size: 17px;
              letter-spacing: 0;
            }

            code {
              border-radius: 6px;
              background: #eef2f7;
              padding: 2px 6px;
              font-size: 0.95em;
            }

            ul {
              margin: 0;
              padding-left: 20px;
              color: #526071;
              line-height: 1.7;
            }

            .status {
              display: inline-flex;
              align-items: center;
              gap: 8px;
              margin-bottom: 18px;
              border: 1px solid #cbd6e2;
              border-radius: 999px;
              background: #fff;
              padding: 8px 12px;
              color: #304255;
              font-size: 14px;
            }

            .dot {
              width: 9px;
              height: 9px;
              border-radius: 50%;
              background: #1f9d6a;
            }
          </style>
        </head>
        <body>
          <main>
            <div class="status"><span class="dot"></span> mock server online</div>
            <h1>Codex App Browser Use x MCP Inspector</h1>
            <p class="lead">
              A small Hono server for screenshots: Browser Use can verify this page,
              while MCP Inspector can connect to the Streamable HTTP endpoint at
              <code>/mcp</code>.
            </p>
            <section class="grid" aria-label="Demo surfaces">
              <article class="panel">
                <h2>Browser Use view</h2>
                <ul>
                  <li>Open <code>http://localhost:8787</code></li>
                  <li>Check visible copy and layout</li>
                  <li>Capture the article screenshot</li>
                </ul>
              </article>
              <article class="panel">
                <h2>MCP Inspector view</h2>
                <ul>
                  <li>Transport: Streamable HTTP</li>
                  <li>URL: <code>http://localhost:8787/mcp</code></li>
                  <li>List tools, resources, and prompts</li>
                </ul>
              </article>
              <article class="panel">
                <h2>Mock capabilities</h2>
                <ul>
                  <li><code>summarize_demo_context</code></li>
                  <li><code>score_article_workflow</code></li>
                  <li><code>mock://article/context</code></li>
                </ul>
              </article>
            </section>
          </main>
        </body>
      </html>`),
  );

  app.get("/health", (c) =>
    c.json({
      status: "ok",
      server: "codex-browser-mcp-mock",
      transport: "streamable-http",
    }),
  );

  app.all("/mcp", async (c) => {
    const mcpServer = createMcpServer();
    const transport = new StreamableHTTPTransport({ enableJsonResponse: true });
    await mcpServer.connect(transport);
    const response = await transport.handleRequest(c);
    await mcpServer.close();
    return response ?? c.body(null, 204);
  });

  return app;
}
