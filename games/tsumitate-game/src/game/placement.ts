import type { Vec3 } from './types'

export const CLICK_CATCHER_RADIUS = 40
export const PLACEMENT_RADIUS = 5.55
export const MAX_PLACEMENT_DRAG = 6

export interface PlacementRay {
  origin: Vec3
  direction: Vec3
}

function projectRayToGround(ray: PlacementRay): Vec3 | null {
  if (Math.abs(ray.direction[1]) < 0.00001) return null
  const distance = -ray.origin[1] / ray.direction[1]
  if (distance < 0) return null
  return [
    ray.origin[0] + ray.direction[0] * distance,
    0,
    ray.origin[2] + ray.direction[2] * distance,
  ]
}

function clampToPlacementCircle(point: Vec3): Vec3 {
  const distance = Math.hypot(point[0], point[2])
  if (distance <= PLACEMENT_RADIUS) return point
  const scale = PLACEMENT_RADIUS / distance
  return [point[0] * scale, point[1], point[2] * scale]
}

export function getElevatedPlacementPoint(
  ray: PlacementRay,
  placementSurfacePoints: readonly Vec3[],
  dragDistance: number,
): Vec3 | null {
  if (dragDistance > MAX_PLACEMENT_DRAG) return null
  if (placementSurfacePoints[0]) return placementSurfacePoints[0]
  const groundPoint = projectRayToGround(ray)
  return groundPoint ? clampToPlacementCircle(groundPoint) : null
}
