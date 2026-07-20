const STORAGE_KEY = "minna-no-monosashi:device-token:v1";

export type DeviceIdentity = { token: string; persistent: boolean };
export type TokenStorage = Pick<Storage, "getItem" | "setItem">;

let temporaryToken: string | null = null;

function createToken(): string {
  const bytes = crypto.getRandomValues(new Uint8Array(24));
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

export function getDeviceIdentity(
  storage: TokenStorage = window.localStorage,
): DeviceIdentity {
  try {
    const existing = storage.getItem(STORAGE_KEY);
    if (existing) return { token: existing, persistent: true };
    const token = createToken();
    storage.setItem(STORAGE_KEY, token);
    return { token, persistent: true };
  } catch {
    temporaryToken ??= createToken();
    return { token: temporaryToken, persistent: false };
  }
}

export function resetTemporaryIdentityForTests(): void {
  temporaryToken = null;
}
