import { env, SELF } from "cloudflare:test";
import { expect, it } from "vitest";
import { seedPublishedQuestion } from "./helpers";

const answer = (
  optionId: string,
  deviceToken = "device-token-a",
): Promise<Response> =>
  SELF.fetch(
    "https://example.test/api/blocks/live/questions/question-1/responses",
    {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-device-token": deviceToken,
      },
      body: JSON.stringify({ optionId }),
    },
  );

it("stores one answer and makes same-answer retries idempotent", async () => {
  await seedPublishedQuestion(env.DB);

  const first = await answer("option-1");
  const retry = await answer("option-1");
  const row = await env.DB
    .prepare("SELECT COUNT(*) AS count FROM responses")
    .first<{ count: number }>();

  expect(first.status).toBe(201);
  expect(retry.status).toBe(200);
  expect(row?.count).toBe(1);
});

it("locks the first answer and validates option ownership", async () => {
  await seedPublishedQuestion(env.DB);
  expect((await answer("option-1")).status).toBe(201);

  const changed = await answer("option-2");
  const foreignOption = await answer("option-3", "device-token-b");

  expect(changed.status).toBe(409);
  expect(await changed.json()).toEqual({
    error: { code: "ANSWER_LOCKED", message: "回答は変更できません" },
  });
  expect(foreignOption.status).toBe(400);
});

it("rejects answers unless the block is published", async () => {
  await seedPublishedQuestion(env.DB);
  await env.DB
    .prepare("UPDATE blocks SET status = 'closed', closed_at = ? WHERE id = ?")
    .bind("2026-07-20T01:00:00.000Z", "live")
    .run();

  const response = await answer("option-1");

  expect(response.status).toBe(409);
  expect((await response.json<{ error: { code: string } }>()).error.code).toBe(
    "BLOCK_NOT_OPEN",
  );
});

it("hides live question results until that device has answered", async () => {
  await seedPublishedQuestion(env.DB);
  const resultsUrl =
    "https://example.test/api/blocks/live/questions/question-1/results";

  const hidden = await SELF.fetch(resultsUrl, {
    headers: { "x-device-token": "device-token-a" },
  });
  expect(hidden.status).toBe(403);

  await answer("option-1");
  const visible = await SELF.fetch(resultsUrl, {
    headers: { "x-device-token": "device-token-a" },
  });
  const body = await visible.json<{
    result: { selectedOptionId: string; totalResponses: number };
  }>();

  expect(visible.status).toBe(200);
  expect(body.result.selectedOptionId).toBe("option-1");
  expect(body.result.totalResponses).toBe(1);
});

it("allocates rounded percentages that total 100", async () => {
  await seedPublishedQuestion(env.DB);
  await env.DB
    .prepare(
      "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
    )
    .bind("option-5", "question-1", "8時", 3, "2026-07-20T00:00:00.000Z")
    .run();

  await answer("option-1", "device-token-a");
  await answer("option-2", "device-token-b");
  const third = await answer("option-5", "device-token-c");
  const body = await third.json<{
    result: { options: { percentage: number }[] };
  }>();

  expect(third.status).toBe(201);
  expect(body.result.options.map((option) => option.percentage)).toEqual([
    34, 33, 33,
  ]);
});

it("returns progress and exposes all results after close", async () => {
  await seedPublishedQuestion(env.DB);
  await answer("option-1");

  const progress = await SELF.fetch(
    "https://example.test/api/blocks/live/progress",
    { headers: { "x-device-token": "device-token-a" } },
  );
  expect(await progress.json()).toEqual({
    answeredQuestionIds: ["question-1"],
  });

  await env.DB
    .prepare("UPDATE blocks SET status = 'closed', closed_at = ? WHERE id = ?")
    .bind("2026-07-20T01:00:00.000Z", "live")
    .run();
  const results = await SELF.fetch(
    "https://example.test/api/blocks/live/results",
  );
  const body = await results.json<{ results: unknown[] }>();

  expect(results.status).toBe(200);
  expect(body.results).toHaveLength(2);
});
