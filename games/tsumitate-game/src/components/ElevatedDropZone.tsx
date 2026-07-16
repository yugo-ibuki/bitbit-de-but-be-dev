import type { ThreeEvent } from '@react-three/fiber'
import { useRapier } from '@react-three/rapier'
import { useState } from 'react'
import { BackSide } from 'three'
import {
  CLICK_CATCHER_RADIUS,
  VERTICAL_CAST_DISTANCE,
  VERTICAL_CAST_HEIGHT,
  getElevatedPlacementPoint,
  getVerticalSurfacePoint,
} from '../game/placement'
import type { ChallengePiece, Vec3 } from '../game/types'
import { PlacementGhost } from './PlacementGhost'

interface ElevatedDropZoneProps {
  currentPiece: ChallengePiece | null
  onPlace: (point: Vec3) => void
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
  const { world, rapier } = useRapier()
  const [previewPoint, setPreviewPoint] = useState<Vec3 | null>(null)

  const resolvePoint = (
    event: ThreeEvent<PointerEvent | MouseEvent>,
    dragDistance: number,
  ): Vec3 | null => {
    const groundPoint = getElevatedPlacementPoint(getRay(event), [], dragDistance)
    if (!groundPoint) return null
    const ray = new rapier.Ray(
      { x: groundPoint[0], y: VERTICAL_CAST_HEIGHT, z: groundPoint[2] },
      { x: 0, y: -1, z: 0 },
    )
    const hit = world.castRay(ray, VERTICAL_CAST_DISTANCE, true)
    return hit
      ? getVerticalSurfacePoint(
          groundPoint,
          VERTICAL_CAST_HEIGHT,
          hit.timeOfImpact,
        )
      : groundPoint
  }

  const handlePointerMove = (event: ThreeEvent<PointerEvent>) => {
    event.stopPropagation()
    setPreviewPoint(resolvePoint(event, 0))
  }

  const handleClick = (event: ThreeEvent<MouseEvent>) => {
    event.stopPropagation()
    const point = resolvePoint(event, event.delta)
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
