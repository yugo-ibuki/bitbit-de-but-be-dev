import type { ThreeEvent } from '@react-three/fiber'
import {
  ELEVATED_DROP_ZONE_HEIGHT,
  ELEVATED_DROP_ZONE_RADIUS,
  getElevatedPlacementPoint,
} from '../game/placement'
import type { Vec3 } from '../game/types'

interface ElevatedDropZoneProps {
  onPlace: (point: Vec3) => void
}

export function ElevatedDropZone({ onPlace }: ElevatedDropZoneProps) {
  const handleClick = (event: ThreeEvent<MouseEvent>) => {
    event.stopPropagation()
    const surfacePoints = event.intersections
      .filter((hit) => hit.object.userData.placementSurface === true)
      .map((hit) => [hit.point.x, hit.point.y, hit.point.z] as Vec3)
    const point = getElevatedPlacementPoint(
      [event.point.x, event.point.y, event.point.z],
      surfacePoints,
      event.delta,
    )
    if (point) onPlace(point)
  }

  return (
    <mesh
      position={[0, ELEVATED_DROP_ZONE_HEIGHT, 0]}
      rotation={[-Math.PI / 2, 0, 0]}
      onClick={handleClick}
    >
      <circleGeometry args={[ELEVATED_DROP_ZONE_RADIUS, 64]} />
      <meshBasicMaterial colorWrite={false} depthWrite={false} />
    </mesh>
  )
}
