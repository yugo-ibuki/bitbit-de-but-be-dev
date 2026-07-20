import { SELF } from "cloudflare:test";
import { expect, it } from "vitest";

async function login(password = "dev-admin", ip = crypto.randomUUID()) {
  return SELF.fetch("https://example.test/api/admin/login", {
    method: "POST",
    headers: {
      "content-type": "application/json",
      "cf-connecting-ip": ip,
    },
    body: JSON.stringify({ password }),
  });
}

it("rejects unauthenticated sessions and incorrect passwords", async () => {
  const session = await SELF.fetch("https://example.test/api/admin/session");
  const incorrect = await login("not-the-password");

  expect(session.status).toBe(401);
  expect(incorrect.status).toBe(401);
});

it("creates a secure admin cookie and CSRF token", async () => {
  const response = await login();
  const body = await response.json<{
    authenticated: boolean;
    csrfToken: string;
  }>();

  expect(response.status).toBe(200);
  expect(response.headers.get("set-cookie")).toMatch(
    /HttpOnly.*Secure.*SameSite=Strict/i,
  );
  expect(body).toEqual({ authenticated: true, csrfToken: expect.any(String) });
});

it("restores a valid session from its signed cookie", async () => {
  const loginResponse = await login();
  const cookie = loginResponse.headers.get("set-cookie")?.split(";")[0];

  const response = await SELF.fetch("https://example.test/api/admin/session", {
    headers: { cookie: cookie ?? "" },
  });

  expect(response.status).toBe(200);
  expect(await response.json()).toEqual({
    authenticated: true,
    csrfToken: expect.any(String),
  });
});

it("rejects state changes with a mismatched Origin or CSRF token", async () => {
  const loginResponse = await login();
  const cookie = loginResponse.headers.get("set-cookie")?.split(";")[0] ?? "";
  const { csrfToken } = await loginResponse.json<{ csrfToken: string }>();

  const wrongOrigin = await SELF.fetch(
    "https://example.test/api/admin/logout",
    {
      method: "POST",
      headers: {
        cookie,
        origin: "https://attacker.example",
        "x-csrf-token": csrfToken,
      },
    },
  );
  const wrongToken = await SELF.fetch(
    "https://example.test/api/admin/logout",
    {
      method: "POST",
      headers: {
        cookie,
        origin: "https://example.test",
        "x-csrf-token": "wrong-token",
      },
    },
  );

  expect(wrongOrigin.status).toBe(403);
  expect(wrongToken.status).toBe(403);
});

it("sets baseline security headers on API responses", async () => {
  const response = await SELF.fetch("https://example.test/api/blocks");

  expect(response.headers.get("x-content-type-options")).toBe("nosniff");
  expect(response.headers.get("content-security-policy")).toContain(
    "default-src 'self'",
  );
});
