import type { Context } from "hono";
import { Hono } from "hono";
import { deleteCookie, getCookie, setCookie } from "hono/cookie";
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

  return routes;
}
