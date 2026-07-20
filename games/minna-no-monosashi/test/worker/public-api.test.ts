import { SELF } from "cloudflare:test";
import { expect, it } from "vitest";

it("returns an empty public block list", async () => {
  const response = await SELF.fetch("https://example.test/api/blocks");

  expect(response.status).toBe(200);
  expect(await response.json()).toEqual({ blocks: [] });
});
