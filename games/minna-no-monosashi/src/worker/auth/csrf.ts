import type { AdminSession } from "./session";
import { AppError } from "../http/errors";

export function assertMutationSecurity(
  request: Request,
  session: AdminSession,
): void {
  const expectedOrigin = new URL(request.url).origin;
  if (request.headers.get("origin") !== expectedOrigin) {
    throw new AppError(403, "INVALID_ORIGIN", "リクエスト元を確認できません");
  }
  if (request.headers.get("x-csrf-token") !== session.csrf) {
    throw new AppError(403, "INVALID_CSRF", "セキュリティトークンが不正です");
  }
}
