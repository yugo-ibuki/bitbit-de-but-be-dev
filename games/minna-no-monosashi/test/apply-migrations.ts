import { applyD1Migrations, env } from "cloudflare:test";
import { beforeAll, beforeEach } from "vitest";

beforeAll(async () => {
  await applyD1Migrations(env.DB, env.TEST_MIGRATIONS);
});

beforeEach(async () => {
  await env.DB.batch([
    env.DB.prepare("DELETE FROM responses"),
    env.DB.prepare("DELETE FROM options"),
    env.DB.prepare("DELETE FROM questions"),
    env.DB.prepare("DELETE FROM blocks"),
  ]);
});
