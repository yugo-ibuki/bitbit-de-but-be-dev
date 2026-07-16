export type RankingStorage = Pick<Storage, 'getItem' | 'setItem'>

export interface RankingEntry {
  id: string
  height: number
  usedCount: number
  completedAt: number
}

const MAX_STORED_ENTRIES = 20

export function getRankingStorageKey(challengeKey: string): string {
  return `tsumitate-game:ranking:v1:${challengeKey}`
}

export function getBrowserRankingStorage(): RankingStorage | undefined {
  try {
    return typeof window === 'undefined' ? undefined : window.localStorage
  } catch {
    return undefined
  }
}

function isRankingEntry(value: unknown): value is RankingEntry {
  if (typeof value !== 'object' || value === null) return false
  const entry = value as Record<string, unknown>
  return (
    typeof entry.id === 'string' &&
    entry.id.length > 0 &&
    typeof entry.height === 'number' &&
    Number.isFinite(entry.height) &&
    entry.height > 0 &&
    typeof entry.usedCount === 'number' &&
    Number.isInteger(entry.usedCount) &&
    entry.usedCount > 0 &&
    typeof entry.completedAt === 'number' &&
    Number.isFinite(entry.completedAt) &&
    entry.completedAt >= 0
  )
}

export function sortRankingEntries(
  entries: readonly RankingEntry[],
): RankingEntry[] {
  return [...entries].sort(
    (a, b) =>
      b.height - a.height ||
      a.usedCount - b.usedCount ||
      a.completedAt - b.completedAt,
  )
}

export function loadRankingEntries(
  challengeKey: string,
  storage: RankingStorage | undefined,
): RankingEntry[] {
  try {
    const value: unknown = JSON.parse(
      storage?.getItem(getRankingStorageKey(challengeKey)) ?? '[]',
    )
    if (!Array.isArray(value) || !value.every(isRankingEntry)) return []
    return sortRankingEntries(value).slice(0, MAX_STORED_ENTRIES)
  } catch {
    return []
  }
}

export function saveRankingEntry(
  challengeKey: string,
  entry: RankingEntry,
  storage: RankingStorage | undefined,
): RankingEntry[] {
  if (!isRankingEntry(entry)) return loadRankingEntries(challengeKey, storage)

  const normalizedEntry = {
    ...entry,
    height: Math.round(entry.height * 100) / 100,
  }
  const entries = sortRankingEntries([
    ...loadRankingEntries(challengeKey, storage),
    normalizedEntry,
  ]).slice(0, MAX_STORED_ENTRIES)

  try {
    storage?.setItem(getRankingStorageKey(challengeKey), JSON.stringify(entries))
  } catch {
    // Keep the result in memory when browser storage is unavailable.
  }

  return entries
}
