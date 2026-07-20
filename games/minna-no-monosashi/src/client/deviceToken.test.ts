import { getDeviceIdentity, resetTemporaryIdentityForTests } from "./deviceToken";

beforeEach(() => resetTemporaryIdentityForTests());

it("reuses a persisted token", () => {
  const values = new Map<string, string>();
  const storage = {
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => values.set(key, value),
  };

  expect(getDeviceIdentity(storage)).toEqual(getDeviceIdentity(storage));
  expect(getDeviceIdentity(storage).persistent).toBe(true);
});

it("falls back to one tab token when localStorage is unavailable", () => {
  const broken = {
    getItem: () => {
      throw new Error("blocked");
    },
    setItem: () => {
      throw new Error("blocked");
    },
  };

  const first = getDeviceIdentity(broken);
  const second = getDeviceIdentity(broken);
  expect(first.persistent).toBe(false);
  expect(first.token).toBe(second.token);
});
