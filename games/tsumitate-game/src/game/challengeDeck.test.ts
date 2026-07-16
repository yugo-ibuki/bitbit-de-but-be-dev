import { describe, expect, it } from 'vitest'
import {
  createDailyChallengeDeck,
  getJapanChallengeKey,
} from './challengeDeck'

describe('daily challenge deck', () => {
  it('uses the calendar date in Japan', () => {
    expect(getJapanChallengeKey(new Date('2026-07-15T15:00:00Z'))).toBe(
      '2026-07-16',
    )
  })

  it('creates the same deck for the same key and a different deck for another key', () => {
    const first = createDailyChallengeDeck('2026-07-16')
    expect(createDailyChallengeDeck('2026-07-16')).toEqual(first)
    expect(createDailyChallengeDeck('2026-07-17')).not.toEqual(first)
  })

  it('creates the required shape and size quotas with an large box first', () => {
    const deck = createDailyChallengeDeck('2026-07-16')
    expect(deck).toHaveLength(100)
    expect(deck[0]).toMatchObject({ kind: 'box', size: 'large' })

    expect(deck.filter((piece) => piece.kind === 'box')).toHaveLength(34)
    expect(deck.filter((piece) => piece.kind === 'sphere')).toHaveLength(33)
    expect(deck.filter((piece) => piece.kind === 'cylinder')).toHaveLength(33)
    expect(deck.filter((piece) => piece.size === 'small')).toHaveLength(30)
    expect(deck.filter((piece) => piece.size === 'medium')).toHaveLength(40)
    expect(deck.filter((piece) => piece.size === 'large')).toHaveLength(30)
  })
})
