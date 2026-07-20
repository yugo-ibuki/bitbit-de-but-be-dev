import { env, SELF } from "cloudflare:test";
import { expect, it } from "vitest";
import { seedBlock, seedPublishedQuestion } from "./helpers";

it("returns an empty public block list", async () => {
  const response = await SELF.fetch("https://example.test/api/blocks");

  expect(response.status).toBe(200);
  expect(await response.json()).toEqual({ blocks: [] });
});

it("lists published and closed blocks but hides drafts", async () => {
  await seedBlock(env.DB, { id: "draft", slug: "draft", status: "draft" });
  await seedBlock(env.DB, { id: "live", slug: "live", status: "published" });
  await seedBlock(env.DB, { id: "closed", slug: "closed", status: "closed" });

  const response = await SELF.fetch("https://example.test/api/blocks");
  const body = await response.json<{ blocks: { slug: string }[] }>();

  expect(response.status).toBe(200);
  expect(body.blocks.map((block) => block.slug)).toEqual(["live", "closed"]);
});

it("returns public questions and options in position order", async () => {
  await seedPublishedQuestion(env.DB);

  const response = await SELF.fetch("https://example.test/api/blocks/live");
  const body = await response.json<{
    block: {
      slug: string;
      questions: { id: string; options: { id: string }[] }[];
    };
  }>();

  expect(response.status).toBe(200);
  expect(body.block.slug).toBe("live");
  expect(body.block.questions.map((question) => question.id)).toEqual([
    "question-1",
    "question-2",
  ]);
  expect(body.block.questions[0]?.options.map((option) => option.id)).toEqual([
    "option-1",
    "option-2",
  ]);
});

it("does not expose draft block details", async () => {
  await seedBlock(env.DB, { id: "draft", slug: "draft", status: "draft" });

  const response = await SELF.fetch("https://example.test/api/blocks/draft");

  expect(response.status).toBe(404);
  expect(await response.json()).toEqual({
    error: { code: "NOT_FOUND", message: "ブロックが見つかりません" },
  });
});
