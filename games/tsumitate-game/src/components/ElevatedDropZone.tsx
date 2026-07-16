import type { ThreeEvent } from '@react-three/fiber'
import { useState } from 'react'
import { BackSide } from 'three'
import {
  CLICK_CATCHER_RADIUS,
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
      <mesh
        position={[0, 4, 0]}
        onPointerMove={handlePointerMove}
        onPointerLeave={() => setPreviewPoint(null)}
        onClick={handleClick}
      >
        <sphereGeometry args={[CLICK_CATCHER_RADIUS, 32, 20]} />
        <meshBasicMaterial
          side={BackSide}
          colorWrite={false}
          depthWrite={false}
        />
      </mesh>
      {currentPiece && previewPoint && (
        <PlacementGhost piece={currentPiece} point={previewPoint} />
      )}
    </>
  )
}
