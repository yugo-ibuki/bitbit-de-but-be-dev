import type { Vec3 } from './types'

export const ELEVATED_DROP_ZONE_HEIGHT = 4.5
export const ELEVATED_DROP_ZONE_RADIUS = 6.3
export const MAX_PLACEMENT_DRAG = 6

export function getElevatedPlacementPoint(
  elevatedPoint: Vec3,
  placementSurfacePoints: readonly Vec3[],
  dragDistance: number,
): Vec3 | null {
  if (dragDistance > MAX_PLACEMENT_DRAG) return null
  return placementSurfacePoints[0] ?? [elevatedPoint[0], 0, elevatedPoint[2]]
}
