import type { ShapeKind } from './types'

export interface HeightBound {
  maxY: number
}

export interface QuaternionLike {
  x: number
  y: number
  z: number
  w: number
}

const BOX_HALF_EXTENTS = { x: 0.85, y: 0.6, z: 0.75 }
const SPHERE_RADIUS = 0.9
const CYLINDER_HALF_HEIGHT = 0.9
const CYLINDER_RADIUS = 0.82

export function canAccumulateStability(
  activeBodyCount: number,
  expectedBodyCount: number,
  allBodiesStable: boolean,
): boolean {
  return (
    activeBodyCount > 0 &&
    activeBodyCount === expectedBodyCount &&
    allBodiesStable
  )
}

export function getObjectTopY(
  kind: ShapeKind,
  centerY: number,
  rotation: QuaternionLike,
  scale = 1,
): number {
  if (kind === 'sphere') return centerY + SPHERE_RADIUS * scale

  const rowY = {
    x: 2 * (rotation.x * rotation.y + rotation.z * rotation.w),
    y: 1 - 2 * (rotation.x * rotation.x + rotation.z * rotation.z),
    z: 2 * (rotation.y * rotation.z - rotation.x * rotation.w),
  }

  if (kind === 'box') {
    const halfExtentY =
      Math.abs(rowY.x) * BOX_HALF_EXTENTS.x +
      Math.abs(rowY.y) * BOX_HALF_EXTENTS.y +
      Math.abs(rowY.z) * BOX_HALF_EXTENTS.z
    return centerY + halfExtentY * scale
  }

  const axisY = Math.min(1, Math.abs(rowY.y))
  const radialY = Math.sqrt(Math.max(0, 1 - axisY * axisY))
  return (
    centerY +
    (CYLINDER_HALF_HEIGHT * axisY + CYLINDER_RADIUS * radialY) * scale
  )
}

export function getTowerHeight(bounds: readonly HeightBound[]): number {
  const highest = bounds.reduce(
    (value, bound) => Math.max(value, bound.maxY),
    0,
  )
  return Math.round(Math.max(0, highest) * 100) / 100
}
