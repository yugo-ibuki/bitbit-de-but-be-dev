import { describe, expect, it } from 'vitest'
import {
  canAccumulateStability,
  getObjectTopY,
  getTowerHeight,
} from './heightMeasurement'

describe('getTowerHeight', () => {
  it('returns the highest collider top rounded to centimeters', () => {
    expect(getTowerHeight([{ maxY: 2.346 }, { maxY: 4.204 }])).toBe(4.2)
  })

  it('clamps empty and below-platform bounds to zero', () => {
    expect(getTowerHeight([])).toBe(0)
    expect(getTowerHeight([{ maxY: -2 }])).toBe(0)
  })

  it('uses each shape extent for an upright object', () => {
    const identity = { x: 0, y: 0, z: 0, w: 1 }

    expect(getObjectTopY('box', 2, identity)).toBeCloseTo(2.6)
    expect(getObjectTopY('sphere', 2, identity)).toBeCloseTo(2.9)
    expect(getObjectTopY('cylinder', 2, identity)).toBeCloseTo(2.9)
  })

  it('accounts for the rigid body rotation', () => {
    const quarterTurnX = {
      x: Math.SQRT1_2,
      y: 0,
      z: 0,
      w: Math.SQRT1_2,
    }

    expect(getObjectTopY('box', 2, quarterTurnX)).toBeCloseTo(2.75)
    expect(getObjectTopY('cylinder', 2, quarterTurnX)).toBeCloseTo(2.82)
  })

  it('does not accumulate stable time before a body is registered', () => {
    expect(canAccumulateStability(0, 0, true)).toBe(false)
    expect(canAccumulateStability(0, 1, true)).toBe(false)
    expect(canAccumulateStability(1, 1, true)).toBe(true)
  })
})
