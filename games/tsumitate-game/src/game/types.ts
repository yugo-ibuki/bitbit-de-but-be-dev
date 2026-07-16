export type ShapeKind = 'box' | 'sphere' | 'cylinder'
export type PieceSize = 'small' | 'medium' | 'large'
export type Vec3 = readonly [number, number, number]

export interface ChallengePiece {
  kind: ShapeKind
  size: PieceSize
  scale: number
  color: string
  rotation: Vec3
}

export interface StackingItem {
  id: string
  kind: ShapeKind
  size: PieceSize
  scale: number
  position: Vec3
  rotation: Vec3
  color: string
}
