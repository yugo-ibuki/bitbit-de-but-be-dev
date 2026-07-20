import { Hono } from "hono";
import { AppError } from "./http/errors";
import { publicRoutes } from "./routes/publicRoutes";
import { responseRoutes } from "./routes/responseRoutes";

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
  app.route("/api", responseRoutes());

  app.onError((error, context) => {
    if (error instanceof AppError) {
      return context.json(
        {
          error: {
            code: error.code,
            message: error.message,
            ...(error.details === undefined ? {} : { details: error.details }),
          },
        },
        error.status,
      );
    }
    console.error("Unhandled request error");
    return context.json(
      { error: { code: "INTERNAL_ERROR", message: "処理に失敗しました" } },
      500,
    );
  });

  return app;
}
