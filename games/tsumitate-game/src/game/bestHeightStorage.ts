export const BEST_HEIGHT_STORAGE_KEY = 'tsumitate-game:best-height:v1'

export type HeightStorage = Pick<Storage, 'getItem' | 'setItem'>

export function getBrowserHeightStorage(): HeightStorage | undefined {
  try {
    return typeof window === 'undefined' ? undefined : window.localStorage
  } catch {
    return undefined
  }
}

export function loadBestHeight(storage: HeightStorage | undefined): number {
  try {
    const value: unknown = JSON.parse(
      storage?.getItem(BEST_HEIGHT_STORAGE_KEY) ?? '0',
    )
    return typeof value === 'number' && Number.isFinite(value) && value >= 0
      ? value
      : 0
  } catch {
    return 0
  }
}

export function saveBestHeight(
  value: number,
  storage: HeightStorage | undefined,
): void {
  if (!Number.isFinite(value) || value < 0) return
  try {
    storage?.setItem(BEST_HEIGHT_STORAGE_KEY, JSON.stringify(value))
  } catch {
    // Keep the in-memory record when browser storage is unavailable.
  }
}
