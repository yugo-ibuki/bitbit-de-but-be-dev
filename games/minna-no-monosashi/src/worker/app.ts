import { Hono } from "hono";
import { publicRoutes } from "./routes/publicRoutes";

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

  app.route("/api", publicRoutes());

  return app;
}
