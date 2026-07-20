import { Hono } from "hono";

export function createApp() {
  const app = new Hono<{ Bindings: Env }>();

  app.use("*", async (context, next) => {
    await next();
    context.header("X-Content-Type-Options", "nosniff");
    context.header("Referrer-Policy", "strict-origin-when-cross-origin");
    context.header(
      "Content-Security-Policy",
      "default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self'; connect-src 'self'",
    );
  });

  app.get("/api/blocks", (context) => context.json({ blocks: [] }));

  return app;
}
