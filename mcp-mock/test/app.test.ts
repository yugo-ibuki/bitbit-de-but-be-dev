import { describe, expect, it } from "vitest";
import { createApp } from "../src/app.js";
import {
  calculateInspectorScore,
  makeDemoSummary,
} from "../src/demo-data.js";
import { articlePromptNames } from "../src/mcp-server.js";

describe("mcp-mock web surface", () => {
  it("renders a screenshot-friendly landing page", async () => {
    const app = createApp();

    const response = await app.request("/");
    const html = await response.text();

    expect(response.status).toBe(200);
    expect(response.headers.get("content-type")).toContain("text/html");
    expect(html).toContain("Codex App Browser Use");
    expect(html).toContain("/mcp");
    expect(html).toContain("MCP Inspector");
  });

  it("returns a stable health payload", async () => {
    const app = createApp();

    const response = await app.request("/health");

    expect(response.status).toBe(200);
    await expect(response.json()).resolves.toEqual({
      status: "ok",
      server: "codex-browser-mcp-mock",
      transport: "streamable-http",
    });
  });
});

describe("demo tool data", () => {
  it("summarizes mock browser and inspector signals", () => {
    expect(makeDemoSummary("browser")).toContain("Browser Use");
    expect(makeDemoSummary("inspector")).toContain("Inspector");
  });

  it("scores the demo workflow from visible signals", () => {
    expect(
      calculateInspectorScore({
        browserChecked: true,
        inspectorConnected: true,
        toolsCalled: 2,
      }),
    ).toEqual({
      score: 95,
      label: "article-ready",
    });
  });
});

describe("mcp prompts", () => {
  it("lists article prompts for Inspector demos", async () => {
    const app = createApp();

    const response = await app.request("/mcp", {
      method: "POST",
      headers: {
        "content-type": "application/json",
        accept: "application/json, text/event-stream",
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "prompts/list",
      }),
    });
    const body = await response.json();
    const names = body.result.prompts.map((prompt: { name: string }) => prompt.name);

    expect(response.status).toBe(200);
    expect(names).toEqual(expect.arrayContaining(articlePromptNames));
    expect(names).toHaveLength(10);
  });

  it("returns a prompt message with the selected tone", async () => {
    const app = createApp();

    const response = await app.request("/mcp", {
      method: "POST",
      headers: {
        "content-type": "application/json",
        accept: "application/json, text/event-stream",
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "prompts/get",
        params: {
          name: "summarize_prompts_tab",
          arguments: { tone: "technical" },
        },
      }),
    });
    const body = await response.json();

    expect(response.status).toBe(200);
    expect(body.result.messages[0].content.text).toContain("prompts/list");
    expect(body.result.messages[0].content.text).toContain("prompts/get");
  });
});
