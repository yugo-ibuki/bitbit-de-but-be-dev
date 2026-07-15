import { describe, expect, it } from 'vitest'
import { createStackingItem, MAX_OBJECTS } from './gameRules'
import { gameReducer, initialGameState } from './gameState'

describe('gameReducer', () => {
  it('selects a shape and adds an item', () => {
    const selected = gameReducer(initialGameState, { type: 'select', kind: 'sphere' })
    const item = createStackingItem('sphere', [0, 0, 0], 'item-1', () => 0.5)
    const placed = gameReducer(selected, { type: 'place', item })
    expect(placed.selectedKind).toBe('sphere')
    expect(placed.items).toEqual([item])
  })

  it('blocks items beyond the object limit', () => {
    let state = initialGameState
    for (let index = 0; index < MAX_OBJECTS; index += 1) {
      const item = createStackingItem('box', [0, 0, 0], 'item-' + index, () => 0.5)
      state = gameReducer(state, { type: 'place', item })
    }
    const overflow = createStackingItem('box', [0, 0, 0], 'overflow', () => 0.5)
    const blocked = gameReducer(state, { type: 'place', item: overflow })
    expect(blocked.items).toHaveLength(MAX_OBJECTS)
    expect(blocked.notice).toContain('100個まで')
  })

  it('triggers destruction only when objects exist', () => {
    expect(gameReducer(initialGameState, { type: 'destroy' }).notice).toBe(
      '先に物体を積んでください',
    )
    const item = createStackingItem('box', [0, 0, 0], 'item-1', () => 0.5)
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    const destroyed = gameReducer(withItem, { type: 'destroy' })
    expect(destroyed.destructionVersion).toBe(1)
    expect(destroyed.containmentEnabled).toBe(false)
  })

  it('removes fallen objects and resets all objects', () => {
    const item = createStackingItem('box', [0, 0, 0], 'item-1', () => 0.5)
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    expect(gameReducer(withItem, { type: 'remove', id: item.id }).items).toEqual([])
    const reset = gameReducer(withItem, { type: 'reset' })
    expect(reset.items).toEqual([])
    expect(reset.containmentEnabled).toBe(true)
  })
})
