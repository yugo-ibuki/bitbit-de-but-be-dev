import { Hono } from "hono";
import { BlockService } from "../services/blockService";

export function publicRoutes() {
  const routes = new Hono<{ Bindings: Env }>();

  routes.get("/blocks", async (context) => {
    const blocks = await new BlockService(context.env.DB).listPublic();
    return context.json({ blocks });
  });

  routes.get("/blocks/:slug", async (context) => {
    const block = await new BlockService(context.env.DB).findPublicBySlug(
      context.req.param("slug"),
    );
    if (!block) {
      return context.json(
        {
          error: {
            code: "NOT_FOUND",
            message: "ブロックが見つかりません",
          },
        },
        404,
      );
    }
    return context.json({ block });
  });

  return routes;
}
