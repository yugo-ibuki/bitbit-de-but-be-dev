import { defineConfig } from "vitest/config";

export default defineConfig({
  build: {
    chunkSizeWarningLimit: 600,
  },
  test: {
    environment: "node",
  },
});
