import type { ChallengePiece, Vec3 } from '../game/types'

interface PlacementGhostProps {
  piece: ChallengePiece
  point: Vec3
}

const halfHeight = {
  box: 0.6,
  sphere: 0.9,
  cylinder: 0.9,
} as const

export function PlacementGhost({ piece, point }: PlacementGhostProps) {
  return (
    <mesh
      position={[
        point[0],
        point[1] + halfHeight[piece.kind] * piece.scale,
        point[2],
      ]}
      rotation={piece.rotation}
      scale={piece.scale}
      raycast={() => undefined}
      renderOrder={10}
    >
      {piece.kind === 'box' && <boxGeometry args={[1.7, 1.2, 1.5]} />}
      {piece.kind === 'sphere' && <sphereGeometry args={[0.9, 24, 18]} />}
      {piece.kind === 'cylinder' && (
        <cylinderGeometry args={[0.82, 0.82, 1.8, 24]} />
      )}
      <meshBasicMaterial
        color="#69e6ff"
        transparent
        opacity={0.42}
        wireframe
        depthWrite={false}
        toneMapped={false}
      />
    </mesh>
  )
}
