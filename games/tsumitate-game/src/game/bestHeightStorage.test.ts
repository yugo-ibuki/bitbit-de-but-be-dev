import { beforeEach, describe, expect, it } from 'vitest'
import {
  getBestHeightStorageKey,
  loadBestHeight,
  saveBestHeight,
} from './bestHeightStorage'

describe('best height storage', () => {
  beforeEach(() => localStorage.clear())

  it('round-trips a finite non-negative record', () => {
    saveBestHeight('2026-07-16', 4.2, localStorage)
    expect(loadBestHeight('2026-07-16', localStorage)).toBe(4.2)
    expect(loadBestHeight('2026-07-17', localStorage)).toBe(0)
  })

  it.each(['broken', '-2', 'null', '"4"'])(
    'rejects invalid value %s',
    (value) => {
      localStorage.setItem(getBestHeightStorageKey('2026-07-16'), value)
      expect(loadBestHeight('2026-07-16', localStorage)).toBe(0)
    },
  )

  it('falls back when storage throws', () => {
    const storage = {
      getItem: () => {
        throw new Error('blocked')
      },
      setItem: () => {
        throw new Error('blocked')
      },
    }

    expect(loadBestHeight('2026-07-16', storage)).toBe(0)
    expect(() => saveBestHeight('2026-07-16', 3.2, storage)).not.toThrow()
  })
})
