import type { ShapeKind, StackingItem, Vec3 } from './types'

export const MAX_OBJECTS = 100
export const FALL_LIMIT_Y = -20

const COLORS = ['#ff6b6b', '#ffd166', '#06d6a0', '#4cc9f0', '#b517ff'] as const

export const SHAPE_CONFIG: Record<
  ShapeKind,
  { label: string; dropOffset: number; collider: 'cuboid' | 'ball' | 'hull' }
> = {
  box: { label: 'ボックス', dropOffset: 3.4, collider: 'cuboid' },
  sphere: { label: 'ボール', dropOffset: 3, collider: 'ball' },
  cylinder: { label: 'シリンダー', dropOffset: 3.3, collider: 'hull' },
}

export function canSpawn(count: number): boolean {
  return count < MAX_OBJECTS
}

export function createStackingItem(
  kind: ShapeKind,
  point: Vec3,
  id: string,
  random: () => number = Math.random,
): StackingItem {
  const angle = () => (random() - 0.5) * 0.36
  const rotation: Vec3 = [angle(), angle(), angle()]
  const colorIndex = Math.min(COLORS.length - 1, Math.floor(random() * COLORS.length))

  return {
    id,
    kind,
    position: [point[0], point[1] + SHAPE_CONFIG[kind].dropOffset, point[2]],
    rotation,
    color: COLORS[colorIndex],
  }
}

export function getExplosionCenter(points: readonly Vec3[]): Vec3 | null {
  if (points.length === 0) return null
  const sum = points.reduce<Vec3>(
    (total, point) => [
      total[0] + point[0],
      total[1] + point[1],
      total[2] + point[2],
    ],
    [0, 0, 0],
  )
  return [sum[0] / points.length, sum[1] / points.length, sum[2] / points.length]
}

export function getExplosionImpulse(position: Vec3, center: Vec3): Vec3 {
  let dx = position[0] - center[0]
  const dy = position[1] - center[1] + 0.8
  let dz = position[2] - center[2]
  if (Math.abs(dx) + Math.abs(dz) < 0.001) {
    dx = 0.24
    dz = 0.16
  }
  const length = Math.hypot(dx, dy, dz)
  const distance = Math.hypot(
    position[0] - center[0],
    position[1] - center[1],
    position[2] - center[2],
  )
  const strength = Math.max(9, 30 - distance * 2.2)
  return [(dx / length) * strength, (dy / length) * strength, (dz / length) * strength]
}

export function isOutOfBounds(y: number): boolean {
  return y < FALL_LIMIT_Y
}
