import { SELF } from "cloudflare:test";
import { expect, it } from "vitest";

const validDraft = {
  title: "日常の境界線",
  slug: "daily-boundaries",
  description: "あなたの境界線を教えてください。",
  questions: [
    { prompt: "朝早いのは？", options: ["6時", "7時", "8時"] },
  ],
};

async function createAdminClient() {
  const login = await SELF.fetch("https://example.test/api/admin/login", {
    method: "POST",
    headers: {
      "content-type": "application/json",
      "cf-connecting-ip": crypto.randomUUID(),
    },
    body: JSON.stringify({ password: "dev-admin" }),
  });
  const cookie = login.headers.get("set-cookie")?.split(";")[0] ?? "";
  const { csrfToken } = await login.json<{ csrfToken: string }>();

  return (path: string, init: RequestInit = {}) =>
    SELF.fetch(`https://example.test/api/admin${path}`, {
      ...init,
      headers: {
        cookie,
        origin: "https://example.test",
        "x-csrf-token": csrfToken,
        ...(init.body ? { "content-type": "application/json" } : {}),
        ...init.headers,
      },
    });
}

it("creates, reads, updates, lists, and deletes a draft", async () => {
  const request = await createAdminClient();
  const created = await request("/blocks", {
    method: "POST",
    body: JSON.stringify(validDraft),
  });
  const createdBody = await created.json<{ block: { id: string } }>();
  expect(created.status).toBe(201);

  const updated = await request(`/blocks/${createdBody.block.id}`, {
    method: "PUT",
    body: JSON.stringify({ ...validDraft, title: "更新したタイトル" }),
  });
  expect((await updated.json<{ block: { title: string } }>()).block.title).toBe(
    "更新したタイトル",
  );

  const detail = await request(`/blocks/${createdBody.block.id}`);
  expect((await detail.json<{ block: { questions: unknown[] } }>()).block.questions).toHaveLength(1);
  const list = await request("/blocks");
  expect((await list.json<{ blocks: unknown[] }>()).blocks).toHaveLength(1);

  expect(
    (await request(`/blocks/${createdBody.block.id}`, { method: "DELETE" }))
      .status,
  ).toBe(204);
});

it("publishes a valid draft and prevents published edits", async () => {
  const request = await createAdminClient();
  const created = await request("/blocks", {
    method: "POST",
    body: JSON.stringify(validDraft),
  });
  const { block } = await created.json<{ block: { id: string } }>();

  const published = await request(`/blocks/${block.id}/publish`, {
    method: "POST",
  });
  expect(published.status).toBe(200);
  expect(
    (await published.json<{ block: { status: string } }>()).block.status,
  ).toBe("published");
  expect(
    (
      await request(`/blocks/${block.id}`, {
        method: "PUT",
        body: JSON.stringify(validDraft),
      })
    ).status,
  ).toBe(409);
  expect(
    (await request(`/blocks/${block.id}`, { method: "DELETE" })).status,
  ).toBe(409);
});

it("rejects invalid drafts when publishing", async () => {
  const request = await createAdminClient();
  for (const [suffix, questions] of [
    ["empty", []],
    ["eleven", Array.from({ length: 11 }, () => validDraft.questions[0])],
    ["duplicate", [{ prompt: "朝早いのは？", options: ["6時", " 6時 "] }]],
  ] as const) {
    const created = await request("/blocks", {
      method: "POST",
      body: JSON.stringify({ ...validDraft, slug: `invalid-${suffix}`, questions }),
    });
    const { block } = await created.json<{ block: { id: string } }>();
    const response = await request(`/blocks/${block.id}/publish`, {
      method: "POST",
    });
    expect(response.status).toBe(422);
  }
});

it("closes published blocks without allowing them to reopen", async () => {
  const request = await createAdminClient();
  const created = await request("/blocks", {
    method: "POST",
    body: JSON.stringify(validDraft),
  });
  const { block } = await created.json<{ block: { id: string } }>();
  await request(`/blocks/${block.id}/publish`, { method: "POST" });

  const closed = await request(`/blocks/${block.id}/close`, { method: "POST" });
  expect(
    (await closed.json<{ block: { status: string } }>()).block.status,
  ).toBe("closed");
  expect(
    (await request(`/blocks/${block.id}/publish`, { method: "POST" })).status,
  ).toBe(409);
});

it("clones a block to uniquely named drafts", async () => {
  const request = await createAdminClient();
  const created = await request("/blocks", {
    method: "POST",
    body: JSON.stringify(validDraft),
  });
  const { block } = await created.json<{ block: { id: string } }>();

  const first = await request(`/blocks/${block.id}/clone`, { method: "POST" });
  const second = await request(`/blocks/${block.id}/clone`, { method: "POST" });

  expect((await first.json<{ block: { slug: string; status: string } }>()).block).toMatchObject({
    slug: "daily-boundaries-copy",
    status: "draft",
  });
  expect((await second.json<{ block: { slug: string } }>()).block.slug).toBe(
    "daily-boundaries-copy-2",
  );
});
