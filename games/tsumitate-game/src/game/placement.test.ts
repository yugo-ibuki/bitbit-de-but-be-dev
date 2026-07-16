import { describe, expect, it } from 'vitest'
import {
  ELEVATED_DROP_ZONE_HEIGHTS,
  PLACEMENT_RADIUS,
  getElevatedPlacementPoint,
} from './placement'

describe('getElevatedPlacementPoint', () => {
  it('projects the camera ray to platform height', () => {
    expect(
      getElevatedPlacementPoint(
        { origin: [4, 10, 2], direction: [-0.2, -1, -0.1] },
        [],
        0,
      ),
    ).toEqual([2, 0, 1])
  })

  it('clamps a ground projection inside the platform circle', () => {
    const point = getElevatedPlacementPoint(
      { origin: [10, 10, 0], direction: [0, -1, 0] },
      [],
      0,
    )
    expect(point?.[0]).toBeCloseTo(PLACEMENT_RADIUS)
    expect(point?.[1]).toBe(0)
    expect(point?.[2]).toBe(0)
  })

  it('preserves the nearest existing placement surface', () => {
    expect(
      getElevatedPlacementPoint(
        { origin: [4, 10, 2], direction: [-0.2, -1, -0.1] },
        [[1, 2.2, 0]],
        0,
      ),
    ).toEqual([1, 2.2, 0])
  })

  it('rejects a camera drag and rays that do not reach the ground', () => {
    expect(
      getElevatedPlacementPoint(
        { origin: [0, 10, 0], direction: [0, -1, 0] },
        [],
        7,
      ),
    ).toBeNull()
    expect(
      getElevatedPlacementPoint(
        { origin: [0, 10, 0], direction: [1, 0, 0] },
        [],
        0,
      ),
    ).toBeNull()
  })

  it('covers the complete vertical placement zone', () => {
    expect(ELEVATED_DROP_ZONE_HEIGHTS).toEqual([3, 6, 9, 12, 15, 18, 21, 24])
  })
})
