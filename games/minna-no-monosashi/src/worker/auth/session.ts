import { constantTimeEqual } from "./crypto";

export const ADMIN_SESSION_COOKIE = "minna_admin_session";
export const ADMIN_SESSION_MAX_AGE_SECONDS = 8 * 60 * 60;

export type AdminSession = {
  exp: number;
  csrf: string;
};

const encoder = new TextEncoder();

function bytesToBase64Url(bytes: Uint8Array): string {
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

function base64UrlToBytes(value: string): Uint8Array {
  const base64 = value.replace(/-/g, "+").replace(/_/g, "/");
  const padded = base64.padEnd(Math.ceil(base64.length / 4) * 4, "=");
  return Uint8Array.from(atob(padded), (character) => character.charCodeAt(0));
}

async function sign(secret: string, value: string): Promise<Uint8Array> {
  const key = await crypto.subtle.importKey(
    "raw",
    encoder.encode(secret),
    { name: "HMAC", hash: "SHA-256" },
    false,
    ["sign"],
  );
  return new Uint8Array(
    await crypto.subtle.sign("HMAC", key, encoder.encode(value)),
  );
}

function randomHex(byteLength: number): string {
  const bytes = crypto.getRandomValues(new Uint8Array(byteLength));
  return Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join(
    "",
  );
}

export async function createSessionToken(secret: string): Promise<{
  token: string;
  session: AdminSession;
}> {
  const session = {
    exp: Math.floor(Date.now() / 1000) + ADMIN_SESSION_MAX_AGE_SECONDS,
    csrf: randomHex(16),
  };
  const payload = bytesToBase64Url(encoder.encode(JSON.stringify(session)));
  const signature = bytesToBase64Url(await sign(secret, payload));
  return { token: `${payload}.${signature}`, session };
}

export async function verifySessionToken(
  token: string | undefined,
  secret: string,
): Promise<AdminSession | null> {
  if (!token) return null;
  const [payload, signatureText, extra] = token.split(".");
  if (!payload || !signatureText || extra) return null;

  try {
    const expectedSignature = await sign(secret, payload);
    if (
      !constantTimeEqual(expectedSignature, base64UrlToBytes(signatureText))
    ) {
      return null;
    }
    const session = JSON.parse(
      new TextDecoder().decode(base64UrlToBytes(payload)),
    ) as Partial<AdminSession>;
    if (
      typeof session.exp !== "number" ||
      session.exp <= Math.floor(Date.now() / 1000) ||
      typeof session.csrf !== "string" ||
      !/^[a-f0-9]{32}$/.test(session.csrf)
    ) {
      return null;
    }
    return session as AdminSession;
  } catch {
    return null;
  }
}
