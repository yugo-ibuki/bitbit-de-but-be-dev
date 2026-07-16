import type { ThreeEvent } from '@react-three/fiber'
import { useState } from 'react'
import {
  ELEVATED_DROP_ZONE_HEIGHTS,
  ELEVATED_DROP_ZONE_RADIUS,
  getElevatedPlacementPoint,
} from '../game/placement'
import type { ChallengePiece, Vec3 } from '../game/types'
import { PlacementGhost } from './PlacementGhost'

interface ElevatedDropZoneProps {
  currentPiece: ChallengePiece | null
  onPlace: (point: Vec3) => void
}

function getSurfacePoints(event: ThreeEvent<PointerEvent | MouseEvent>): Vec3[] {
  return event.intersections
    .filter((hit) => hit.object.userData.placementSurface === true)
    .map((hit) => [hit.point.x, hit.point.y, hit.point.z] as Vec3)
}

function getRay(event: ThreeEvent<PointerEvent | MouseEvent>) {
  return {
    origin: [event.ray.origin.x, event.ray.origin.y, event.ray.origin.z] as Vec3,
    direction: [
      event.ray.direction.x,
      event.ray.direction.y,
      event.ray.direction.z,
    ] as Vec3,
  }
}

export function ElevatedDropZone({
  currentPiece,
  onPlace,
}: ElevatedDropZoneProps) {
  const [previewPoint, setPreviewPoint] = useState<Vec3 | null>(null)

  const handlePointerMove = (event: ThreeEvent<PointerEvent>) => {
    event.stopPropagation()
    setPreviewPoint(
      getElevatedPlacementPoint(getRay(event), getSurfacePoints(event), 0),
    )
  }

  const handleClick = (event: ThreeEvent<MouseEvent>) => {
    event.stopPropagation()
    const point = getElevatedPlacementPoint(
      getRay(event),
      getSurfacePoints(event),
      event.delta,
    )
    if (!point) return
    setPreviewPoint(point)
    onPlace(point)
  }

  return (
    <>
      <group
        onPointerMove={handlePointerMove}
        onPointerLeave={() => setPreviewPoint(null)}
        onClick={handleClick}
      >
        {ELEVATED_DROP_ZONE_HEIGHTS.map((height) => (
          <mesh
            key={height}
            position={[0, height, 0]}
            rotation={[-Math.PI / 2, 0, 0]}
          >
            <circleGeometry args={[ELEVATED_DROP_ZONE_RADIUS, 64]} />
            <meshBasicMaterial colorWrite={false} depthWrite={false} />
          </mesh>
        ))}
      </group>
      {currentPiece && previewPoint && (
        <PlacementGhost piece={currentPiece} point={previewPoint} />
      )}
    </>
  )
}
