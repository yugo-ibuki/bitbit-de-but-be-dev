import { beforeEach, describe, expect, it } from 'vitest'
import {
  getRankingStorageKey,
  loadRankingEntries,
  saveRankingEntry,
} from './rankingStorage'

describe('ranking storage', () => {
  beforeEach(() => localStorage.clear())

  it('isolates challenge days and ranks height before efficiency and time', () => {
    saveRankingEntry(
      '2026-07-17',
      { id: 'low', height: 4.2, usedCount: 15, completedAt: 100 },
      localStorage,
    )
    saveRankingEntry(
      '2026-07-17',
      { id: 'many', height: 5.1, usedCount: 25, completedAt: 100 },
      localStorage,
    )
    saveRankingEntry(
      '2026-07-17',
      { id: 'late', height: 5.1, usedCount: 20, completedAt: 200 },
      localStorage,
    )
    saveRankingEntry(
      '2026-07-17',
      { id: 'winner', height: 5.1, usedCount: 20, completedAt: 50 },
      localStorage,
    )
    saveRankingEntry(
      '2026-07-18',
      { id: 'tomorrow', height: 8, usedCount: 10, completedAt: 10 },
      localStorage,
    )

    expect(
      loadRankingEntries('2026-07-17', localStorage).map(({ id }) => id),
    ).toEqual(['winner', 'late', 'many', 'low'])
  })

  it('rounds heights and keeps only the best 20 attempts', () => {
    for (let index = 0; index < 25; index += 1) {
      saveRankingEntry(
        '2026-07-17',
        {
          id: `entry-${index}`,
          height: index + 0.126,
          usedCount: 30,
          completedAt: index,
        },
        localStorage,
      )
    }

    const entries = loadRankingEntries('2026-07-17', localStorage)
    expect(entries).toHaveLength(20)
    expect(entries[0]).toMatchObject({ id: 'entry-24', height: 24.13 })
    expect(entries.at(-1)?.id).toBe('entry-5')
  })

  it.each([
    'broken',
    '{}',
    '[null]',
    '[{"id":"bad","height":-1,"usedCount":2,"completedAt":3}]',
    '[{"id":"bad","height":2,"usedCount":0,"completedAt":3}]',
  ])('rejects invalid stored value %s', (value) => {
    localStorage.setItem(getRankingStorageKey('2026-07-17'), value)
    expect(loadRankingEntries('2026-07-17', localStorage)).toEqual([])
  })

  it('falls back safely when storage is unavailable', () => {
    const storage = {
      getItem: () => {
        throw new Error('blocked')
      },
      setItem: () => {
        throw new Error('blocked')
      },
    }

    expect(loadRankingEntries('2026-07-17', storage)).toEqual([])
    expect(
      saveRankingEntry(
        '2026-07-17',
        { id: 'a', height: 2, usedCount: 1, completedAt: 3 },
        storage,
      ),
    ).toEqual([
      { id: 'a', height: 2, usedCount: 1, completedAt: 3 },
    ])
  })
})
