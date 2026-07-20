import type { Context } from "hono";
import { Hono } from "hono";
import { deleteCookie, getCookie, setCookie } from "hono/cookie";
import type { DraftBlockInput } from "../../domain/types";
import { assertMutationSecurity } from "../auth/csrf";
import { hmacHex, verifyPasswordHash } from "../auth/crypto";
import {
  ADMIN_SESSION_COOKIE,
  ADMIN_SESSION_MAX_AGE_SECONDS,
  type AdminSession,
  createSessionToken,
  verifySessionToken,
} from "../auth/session";
import { AppError } from "../http/errors";
import { BlockService } from "../services/blockService";
import { ResponseService } from "../services/responseService";

type AdminContext = Context<{ Bindings: Env }>;

export async function requireAdminSession(
  context: AdminContext,
): Promise<AdminSession> {
  const session = await verifySessionToken(
    getCookie(context, ADMIN_SESSION_COOKIE),
    context.env.SESSION_SECRET,
  );
  if (!session) {
    throw new AppError(401, "UNAUTHENTICATED", "管理者ログインが必要です");
  }
  return session;
}

async function requireAdminMutation(context: AdminContext): Promise<void> {
  const session = await requireAdminSession(context);
  assertMutationSecurity(context.req.raw, session);
}

async function readDraftInput(context: AdminContext): Promise<DraftBlockInput> {
  const value = await context.req.json<unknown>().catch(() => null);
  if (!value || typeof value !== "object") {
    throw new AppError(400, "INVALID_REQUEST", "入力内容が不正です");
  }
  const candidate = value as Record<string, unknown>;
  if (
    typeof candidate.title !== "string" ||
    typeof candidate.slug !== "string" ||
    typeof candidate.description !== "string" ||
    !Array.isArray(candidate.questions) ||
    !candidate.questions.every(
      (question) =>
        question !== null &&
        typeof question === "object" &&
        typeof (question as Record<string, unknown>).prompt === "string" &&
        Array.isArray((question as Record<string, unknown>).options) &&
        ((question as Record<string, unknown>).options as unknown[]).every(
          (option) => typeof option === "string",
        ),
    )
  ) {
    throw new AppError(400, "INVALID_REQUEST", "入力内容が不正です");
  }
  return candidate as DraftBlockInput;
}

export function adminRoutes() {
  const routes = new Hono<{ Bindings: Env }>();

  routes.post("/login", async (context) => {
    const ipKey = await hmacHex(
      context.env.SESSION_SECRET,
      context.req.header("cf-connecting-ip") ?? "unknown-client",
    );
    const rateLimit = await context.env.LOGIN_RATE_LIMITER.limit({ key: ipKey });
    if (!rateLimit.success) {
      throw new AppError(429, "RATE_LIMITED", "しばらく待ってからお試しください");
    }

    const body = await context.req
      .json<{ password?: unknown }>()
      .catch(() => null);
    if (
      !body ||
      typeof body.password !== "string" ||
      !(await verifyPasswordHash(
        body.password,
        context.env.ADMIN_PASSWORD_HASH,
      ))
    ) {
      throw new AppError(401, "INVALID_CREDENTIALS", "パスワードが違います");
    }

    const { token, session } = await createSessionToken(
      context.env.SESSION_SECRET,
    );
    setCookie(context, ADMIN_SESSION_COOKIE, token, {
      httpOnly: true,
      secure: true,
      sameSite: "Strict",
      path: "/",
      maxAge: ADMIN_SESSION_MAX_AGE_SECONDS,
    });
    return context.json({ authenticated: true, csrfToken: session.csrf });
  });

  routes.get("/session", async (context) => {
    const session = await requireAdminSession(context);
    return context.json({ authenticated: true, csrfToken: session.csrf });
  });

  routes.post("/logout", async (context) => {
    const session = await requireAdminSession(context);
    assertMutationSecurity(context.req.raw, session);
    deleteCookie(context, ADMIN_SESSION_COOKIE, { path: "/", secure: true });
    return context.json({ authenticated: false });
  });

  routes.get("/blocks", async (context) => {
    await requireAdminSession(context);
    return context.json({
      blocks: await new BlockService(context.env.DB).listAdmin(),
    });
  });

  routes.post("/blocks", async (context) => {
    await requireAdminMutation(context);
    const block = await new BlockService(context.env.DB).createDraft(
      await readDraftInput(context),
    );
    return context.json({ block }, 201);
  });

  routes.get("/blocks/:id", async (context) => {
    await requireAdminSession(context);
    const block = await new BlockService(context.env.DB).findAdminById(
      context.req.param("id"),
    );
    return context.json({ block });
  });

  routes.put("/blocks/:id", async (context) => {
    await requireAdminMutation(context);
    const block = await new BlockService(context.env.DB).updateDraft(
      context.req.param("id"),
      await readDraftInput(context),
    );
    return context.json({ block });
  });

  routes.delete("/blocks/:id", async (context) => {
    await requireAdminMutation(context);
    await new BlockService(context.env.DB).deleteDraft(context.req.param("id"));
    return context.body(null, 204);
  });

  routes.post("/blocks/:id/publish", async (context) => {
    await requireAdminMutation(context);
    const block = await new BlockService(context.env.DB).publish(
      context.req.param("id"),
    );
    return context.json({ block });
  });

  routes.post("/blocks/:id/close", async (context) => {
    await requireAdminMutation(context);
    const block = await new BlockService(context.env.DB).close(
      context.req.param("id"),
    );
    return context.json({ block });
  });

  routes.post("/blocks/:id/clone", async (context) => {
    await requireAdminMutation(context);
    const block = await new BlockService(context.env.DB).clone(
      context.req.param("id"),
    );
    return context.json({ block }, 201);
  });

  routes.get("/blocks/:id/results", async (context) => {
    await requireAdminSession(context);
    const block = await new BlockService(context.env.DB).findAdminById(
      context.req.param("id"),
    );
    const responseService = new ResponseService(context.env.DB);
    const [results, participantCount] = await Promise.all([
      responseService.adminBlockResults(block.id),
      responseService.adminParticipantCount(block.id),
    ]);
    return context.json({ results, participantCount });
  });

  return routes;
}
