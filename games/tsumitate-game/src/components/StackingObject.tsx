import { useEffect, useRef } from 'react'
import { useFrame, type ThreeEvent } from '@react-three/fiber'
import { RigidBody, type RapierRigidBody } from '@react-three/rapier'
import { SHAPE_CONFIG, isOutOfBounds } from '../game/gameRules'
import type { StackingItem } from '../game/types'

interface StackingObjectProps {
  item: StackingItem
  onPlace: (event: ThreeEvent<MouseEvent>) => void
  onRemove: (id: string) => void
  registerBody: (id: string, body: RapierRigidBody | null) => void
}

export function StackingObject({
  item,
  onPlace,
  onRemove,
  registerBody,
}: StackingObjectProps) {
  const bodyRef = useRef<RapierRigidBody>(null)
  const removed = useRef(false)

  useEffect(() => {
    registerBody(item.id, bodyRef.current)
    return () => registerBody(item.id, null)
  }, [item.id, registerBody])

  useFrame(() => {
    const body = bodyRef.current
    if (!removed.current && body && isOutOfBounds(body.translation().y)) {
      removed.current = true
      onRemove(item.id)
    }
  })

  return (
    <RigidBody
      ref={bodyRef}
      position={item.position}
      rotation={item.rotation}
      colliders={SHAPE_CONFIG[item.kind].collider}
      restitution={0.18}
      friction={0.82}
      linearDamping={0.12}
      angularDamping={0.18}
      ccd
    >
      <mesh
        castShadow
        receiveShadow
        scale={item.scale}
        userData={{ placementSurface: true }}
        onClick={onPlace}
      >
        {item.kind === 'box' && <boxGeometry args={[1.7, 1.2, 1.5]} />}
        {item.kind === 'sphere' && <sphereGeometry args={[0.9, 32, 24]} />}
        {item.kind === 'cylinder' && (
          <cylinderGeometry args={[0.82, 0.82, 1.8, 32]} />
        )}
        <meshStandardMaterial
          color={item.color}
          roughness={0.4}
          metalness={0.12}
          emissive={item.color}
          emissiveIntensity={0.04}
        />
      </mesh>
    </RigidBody>
  )
}
