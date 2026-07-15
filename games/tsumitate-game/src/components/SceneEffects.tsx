import { useEffect, useRef } from 'react'
import { useFrame, useThree } from '@react-three/fiber'
import type { Mesh, MeshBasicMaterial } from 'three'
import type { Vec3 } from '../game/types'

export function Shockwave({ center }: { center: Vec3 }) {
  const mesh = useRef<Mesh>(null)
  const material = useRef<MeshBasicMaterial>(null)
  const elapsed = useRef(0)

  useFrame((_, delta) => {
    elapsed.current += delta
    const progress = Math.min(1, elapsed.current / 0.65)
    mesh.current?.scale.setScalar(0.5 + progress * 9)
    if (material.current) {
      material.current.opacity = (1 - progress) * 0.85
    }
  })

  return (
    <mesh
      ref={mesh}
      position={[center[0], Math.max(0.08, center[1]), center[2]]}
      rotation={[-Math.PI / 2, 0, 0]}
    >
      <ringGeometry args={[0.75, 1, 64]} />
      <meshBasicMaterial
        ref={material}
        color="#ffcf5c"
        transparent
        depthWrite={false}
      />
    </mesh>
  )
}

export function CameraJolt({ trigger }: { trigger: number }) {
  const camera = useThree((state) => state.camera)
  const remaining = useRef(0)
  const previous = useRef({ x: 0, y: 0, z: 0 })

  useEffect(() => {
    if (trigger > 0) {
      remaining.current = 0.42
    }
  }, [trigger])

  useFrame((_, delta) => {
    camera.position.x -= previous.current.x
    camera.position.y -= previous.current.y
    camera.position.z -= previous.current.z
    previous.current = { x: 0, y: 0, z: 0 }

    if (remaining.current <= 0) return
    remaining.current = Math.max(0, remaining.current - delta)
    const strength = remaining.current * 0.16
    previous.current = {
      x: (Math.random() - 0.5) * strength,
      y: (Math.random() - 0.5) * strength,
      z: (Math.random() - 0.5) * strength,
    }
    camera.position.x += previous.current.x
    camera.position.y += previous.current.y
    camera.position.z += previous.current.z
  })

  return null
}
