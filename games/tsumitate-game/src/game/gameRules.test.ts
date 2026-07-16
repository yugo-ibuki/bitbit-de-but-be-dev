import { describe, expect, it } from 'vitest'
import {
  FALL_LIMIT_Y,
  MAX_OBJECTS,
  canSpawn,
  createContainmentSegments,
  createStackingItem,
  getExplosionCenter,
  getExplosionImpulse,
  isOutOfBounds,
} from './gameRules'

describe('gameRules', () => {
  it('blocks spawning at the object limit', () => {
    expect(canSpawn(MAX_OBJECTS - 1)).toBe(true)
    expect(canSpawn(MAX_OBJECTS)).toBe(false)
  })

  it('creates an item above the selected point', () => {
    const item = createStackingItem(
      {
        kind: 'sphere',
        size: 'medium',
        scale: 1,
        color: '#fff',
        rotation: [0.1, 0.2, 0.3],
      },
      [1, 2, 3],
      'item-1',
    )
    expect(item.position).toEqual([1, 5, 3])
    expect(item.rotation).toEqual([0.1, 0.2, 0.3])
    expect(item.size).toBe('medium')
    expect(item.scale).toBe(1)
  })

  it('calculates the average explosion center', () => {
    expect(getExplosionCenter([[0, 0, 0], [2, 4, 6]])).toEqual([1, 2, 3])
    expect(getExplosionCenter([])).toBeNull()
  })

  it('pushes away from the center with an upward component', () => {
    const impulse = getExplosionImpulse([5, 0, 0], [0, 0, 0])
    expect(impulse[0]).toBeGreaterThan(0)
    expect(impulse[1]).toBeGreaterThan(0)
    expect(impulse[2]).toBeCloseTo(0)
  })

  it('detects objects below the fall limit', () => {
    expect(isOutOfBounds(FALL_LIMIT_Y - 0.01)).toBe(true)
    expect(isOutOfBounds(FALL_LIMIT_Y)).toBe(false)
  })

  it('places collider segments around the stage edge', () => {
    const segments = createContainmentSegments(24, 6.65)
    expect(segments).toHaveLength(24)
    for (const segment of segments) {
      expect(Math.hypot(segment.position[0], segment.position[2])).toBeCloseTo(6.65)
      expect(segment.position[1]).toBe(2.4)
    }
  })
})
