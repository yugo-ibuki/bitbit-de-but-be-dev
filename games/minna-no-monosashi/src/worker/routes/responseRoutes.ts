import { Hono } from "hono";
import { hmacHex } from "../auth/crypto";
import { AppError } from "../http/errors";
import { ResponseService } from "../services/responseService";

async function getVoterHash(
  token: string | undefined,
  secret: string,
  required: boolean,
): Promise<string | null> {
  if (!token) {
    if (required) {
      throw new AppError(400, "DEVICE_TOKEN_REQUIRED", "端末トークンが必要です");
    }
    return null;
  }
  if (token.length < 8 || token.length > 256) {
    throw new AppError(400, "INVALID_DEVICE_TOKEN", "端末トークンが不正です");
  }
  return hmacHex(secret, token);
}

export function responseRoutes() {
  const routes = new Hono<{ Bindings: Env }>();

  routes.post("/blocks/:slug/questions/:questionId/responses", async (context) => {
    const voterHash = await getVoterHash(
      context.req.header("x-device-token"),
      context.env.VOTER_HASH_SECRET,
      true,
    );
    if (!voterHash) throw new Error("Required voter hash was not created");

    const rateLimit = await context.env.VOTE_RATE_LIMITER.limit({
      key: voterHash,
    });
    if (!rateLimit.success) {
      throw new AppError(429, "RATE_LIMITED", "しばらく待ってからお試しください");
    }

    const body = await context.req
      .json<{ optionId?: unknown }>()
      .catch(() => null);
    if (!body || typeof body.optionId !== "string") {
      throw new AppError(400, "INVALID_REQUEST", "選択肢を指定してください");
    }

    const result = await new ResponseService(context.env.DB).answer({
      slug: context.req.param("slug"),
      questionId: context.req.param("questionId"),
      optionId: body.optionId,
      voterHash,
    });
    return context.json({ result }, result.created ? 201 : 200);
  });

  routes.get("/blocks/:slug/questions/:questionId/results", async (context) => {
    const voterHash = await getVoterHash(
      context.req.header("x-device-token"),
      context.env.VOTER_HASH_SECRET,
      false,
    );
    const result = await new ResponseService(context.env.DB).questionResult({
      slug: context.req.param("slug"),
      questionId: context.req.param("questionId"),
      voterHash,
    });
    return context.json({ result });
  });

  routes.get("/blocks/:slug/progress", async (context) => {
    const voterHash = await getVoterHash(
      context.req.header("x-device-token"),
      context.env.VOTER_HASH_SECRET,
      true,
    );
    if (!voterHash) throw new Error("Required voter hash was not created");
    const answeredQuestionIds = await new ResponseService(
      context.env.DB,
    ).progress(context.req.param("slug"), voterHash);
    return context.json({ answeredQuestionIds });
  });

  routes.get("/blocks/:slug/results", async (context) => {
    const results = await new ResponseService(
      context.env.DB,
    ).closedBlockResults(context.req.param("slug"));
    return context.json({ results });
  });

  return routes;
}
