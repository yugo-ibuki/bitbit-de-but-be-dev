import { Hono } from "hono";
import { AppError } from "./http/errors";
import { securityHeaders } from "./http/securityHeaders";
import { adminRoutes } from "./routes/adminRoutes";
import { publicRoutes } from "./routes/publicRoutes";
import { responseRoutes } from "./routes/responseRoutes";

export function createApp() {
  const app = new Hono<{ Bindings: Env }>();

  app.use("*", securityHeaders());

  app.route("/api", publicRoutes());
  app.route("/api", responseRoutes());
  app.route("/api/admin", adminRoutes());

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
