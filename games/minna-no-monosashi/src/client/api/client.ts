const CSRF_STORAGE_KEY = "minna-no-monosashi:admin-csrf:v1";
let memoryCsrfToken: string | null = null;

export class ApiError extends Error {
  constructor(
    readonly status: number,
    readonly code: string,
    message: string,
    readonly details?: unknown,
  ) {
    super(message);
    this.name = "ApiError";
  }
}

export async function apiRequest<T>(
  path: string,
  init: RequestInit = {},
  deviceToken?: string,
): Promise<T> {
  const headers = new Headers(init.headers);
  if (init.body && !headers.has("content-type")) {
    headers.set("content-type", "application/json");
  }
  if (deviceToken) headers.set("x-device-token", deviceToken);

  const response = await fetch(path, {
    ...init,
    credentials: "same-origin",
    headers,
  });
  const body = response.status === 204 ? null : await response.json();
  if (!response.ok) {
    const error = body as {
      error?: { code?: string; message?: string; details?: unknown };
    };
    throw new ApiError(
      response.status,
      error.error?.code ?? "REQUEST_FAILED",
      error.error?.message ?? "通信に失敗しました",
      error.error?.details,
    );
  }
  return body as T;
}

export function storeCsrfToken(token: string): void {
  memoryCsrfToken = token;
  try {
    window.sessionStorage.setItem(CSRF_STORAGE_KEY, token);
  } catch {
    // The in-memory token still supports this tab.
  }
}

export function getCsrfToken(): string | null {
  if (memoryCsrfToken) return memoryCsrfToken;
  try {
    memoryCsrfToken = window.sessionStorage.getItem(CSRF_STORAGE_KEY);
  } catch {
    return null;
  }
  return memoryCsrfToken;
}

export function clearCsrfToken(): void {
  memoryCsrfToken = null;
  try {
    window.sessionStorage.removeItem(CSRF_STORAGE_KEY);
  } catch {
    // Nothing else to clear.
  }
}

export function adminRequest<T>(
  path: string,
  init: RequestInit = {},
): Promise<T> {
  const headers = new Headers(init.headers);
  if (init.method && !["GET", "HEAD"].includes(init.method.toUpperCase())) {
    const csrfToken = getCsrfToken();
    if (csrfToken) headers.set("x-csrf-token", csrfToken);
  }
  return apiRequest<T>(`/api/admin${path}`, { ...init, headers });
}
