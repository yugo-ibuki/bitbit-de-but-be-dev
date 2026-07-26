import { env } from "cloudflare:workers";
import { createExecutionContext } from "cloudflare:test";
import { expect, it } from "vitest";
import worker from "../src/index";

it("dispatches HTTP requests to the Hono app without listen()", async () => {
  const response = await worker.fetch(
    new Request("https://example.com/health"),
    env,
    createExecutionContext(),
  );

  expect(response.status).toBe(200);
  expect(await response.json()).toEqual({
    status: "ok",
    service: "workers-sandbox-api",
  });
});
