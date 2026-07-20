import path from "node:path";
import {
  cloudflareTest,
  readD1Migrations,
} from "@cloudflare/vitest-pool-workers";
import { defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [
    cloudflareTest(async () => ({
      wrangler: { configPath: "./wrangler.jsonc" },
      miniflare: {
        bindings: {
          ADMIN_PASSWORD_HASH:
            "pbkdf2-sha256$100000$bWlubmEtbm8tbW9ub3Nhc2hpLWRldg==$PnugNfLn0ZH0ipIIMRZRuG6XtVIDeg7ty6MKPNnbqdU=",
          SESSION_SECRET: "test-session-secret",
          VOTER_HASH_SECRET: "test-voter-secret",
          TEST_MIGRATIONS: await readD1Migrations(
            path.join(import.meta.dirname, "migrations"),
          ),
        },
      },
    })),
  ],
  test: {
    setupFiles: ["./test/apply-migrations.ts"],
    include: ["test/worker/**/*.test.ts"],
  },
});
