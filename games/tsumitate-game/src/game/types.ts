export type ShapeKind = 'box' | 'sphere' | 'cylinder'
export type Vec3 = readonly [number, number, number]

export interface StackingItem {
  id: string
  kind: ShapeKind
  position: Vec3
  rotation: Vec3
  color: string
}
