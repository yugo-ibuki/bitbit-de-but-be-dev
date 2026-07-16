import { describe, expect, it } from 'vitest'
import { getElevatedPlacementPoint } from './placement'

describe('getElevatedPlacementPoint', () => {
  it('projects an empty elevated hit to platform height', () => {
    expect(getElevatedPlacementPoint([2, 4.5, -1], [], 0)).toEqual([2, 0, -1])
  })

  it('preserves the nearest existing placement surface', () => {
    expect(
      getElevatedPlacementPoint([2, 4.5, -1], [[1, 2.2, 0]], 0),
    ).toEqual([1, 2.2, 0])
  })

  it('rejects a camera drag', () => {
    expect(getElevatedPlacementPoint([2, 4.5, -1], [], 7)).toBeNull()
  })
})
