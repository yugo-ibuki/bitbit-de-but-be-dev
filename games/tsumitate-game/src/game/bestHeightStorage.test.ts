import { beforeEach, describe, expect, it } from 'vitest'
import {
  BEST_HEIGHT_STORAGE_KEY,
  loadBestHeight,
  saveBestHeight,
} from './bestHeightStorage'

describe('best height storage', () => {
  beforeEach(() => localStorage.clear())

  it('round-trips a finite non-negative record', () => {
    saveBestHeight(4.2, localStorage)
    expect(loadBestHeight(localStorage)).toBe(4.2)
  })

  it.each(['broken', '-2', 'null', '"4"'])(
    'rejects invalid value %s',
    (value) => {
      localStorage.setItem(BEST_HEIGHT_STORAGE_KEY, value)
      expect(loadBestHeight(localStorage)).toBe(0)
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

    expect(loadBestHeight(storage)).toBe(0)
    expect(() => saveBestHeight(3.2, storage)).not.toThrow()
  })
})
