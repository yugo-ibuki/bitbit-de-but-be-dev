import { describe, expect, it } from 'vitest'
import { createStackingItem, MAX_OBJECTS } from './gameRules'
import { gameReducer, initialGameState } from './gameState'

describe('gameReducer', () => {
  it('selects a shape and adds an item', () => {
    const item = createStackingItem(
      { kind: 'sphere', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
      [0, 0, 0],
      'item-1',
    )
    const placed = gameReducer(initialGameState, { type: 'place', item })
    expect(placed.items).toEqual([item])
    expect(placed.usedCount).toBe(1)
  })

  it('blocks items beyond the object limit', () => {
    let state = initialGameState
    for (let index = 0; index < MAX_OBJECTS; index += 1) {
      const item = createStackingItem(
        { kind: 'box', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
        [0, 0, 0],
        'item-' + index,
      )
      state = gameReducer(state, { type: 'place', item })
    }
    const overflow = createStackingItem(
      { kind: 'box', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
      [0, 0, 0],
      'overflow',
    )
    const blocked = gameReducer(state, { type: 'place', item: overflow })
    expect(blocked.items).toHaveLength(MAX_OBJECTS)
    expect(blocked.notice).toContain('100個まで')
  })

  it('triggers destruction only when objects exist', () => {
    expect(gameReducer(initialGameState, { type: 'destroy' }).notice).toBe(
      '先に物体を積んでください',
    )
    const item = createStackingItem(
      { kind: 'box', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
      [0, 0, 0],
      'item-1',
    )
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    const destroyed = gameReducer(withItem, { type: 'destroy' })
    expect(destroyed.destructionVersion).toBe(1)
    expect(destroyed.containmentEnabled).toBe(false)
    const restored = gameReducer(destroyed, { type: 'restore-containment' })
    expect(restored.containmentEnabled).toBe(true)
  })

  it('removes fallen objects and resets all objects', () => {
    const item = createStackingItem(
      { kind: 'box', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
      [0, 0, 0],
      'item-1',
    )
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    const removed = gameReducer(withItem, { type: 'remove', id: item.id })
    expect(removed.items).toEqual([])
    expect(removed.usedCount).toBe(1)
    const reset = gameReducer(withItem, { type: 'reset' })
    expect(reset.items).toEqual([])
    expect(reset.usedCount).toBe(0)
    expect(reset.containmentEnabled).toBe(true)
  })
})
