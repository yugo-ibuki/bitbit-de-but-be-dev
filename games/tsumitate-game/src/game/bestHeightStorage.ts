export type HeightStorage = Pick<Storage, 'getItem' | 'setItem'>

export function getBestHeightStorageKey(challengeKey: string): string {
  return `tsumitate-game:best-height:v2:${challengeKey}`
}

export function getBrowserHeightStorage(): HeightStorage | undefined {
  try {
    return typeof window === 'undefined' ? undefined : window.localStorage
  } catch {
    return undefined
  }
}

export function loadBestHeight(
  challengeKey: string,
  storage: HeightStorage | undefined,
): number {
  try {
    const value: unknown = JSON.parse(
      storage?.getItem(getBestHeightStorageKey(challengeKey)) ?? '0',
    )
    return typeof value === 'number' && Number.isFinite(value) && value >= 0
      ? value
      : 0
  } catch {
    return 0
  }
}

export function saveBestHeight(
  challengeKey: string,
  value: number,
  storage: HeightStorage | undefined,
): void {
  if (!Number.isFinite(value) || value < 0) return
  try {
    storage?.setItem(getBestHeightStorageKey(challengeKey), JSON.stringify(value))
  } catch {
    // Keep the in-memory record when browser storage is unavailable.
  }
}
