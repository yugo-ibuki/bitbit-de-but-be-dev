import {
  Suspense,
  useCallback,
  useEffect,
  useRef,
  useState,
} from 'react'
import { Canvas, type ThreeEvent } from '@react-three/fiber'
import { OrbitControls, Stars } from '@react-three/drei'
import {
  CylinderCollider,
  Physics,
  RigidBody,
  type RapierRigidBody,
} from '@react-three/rapier'
import { getExplosionCenter, getExplosionImpulse } from '../game/gameRules'
import type { StackingItem, Vec3 } from '../game/types'
import { CameraJolt, Shockwave } from './SceneEffects'
import { StackingObject } from './StackingObject'

interface GameSceneProps {
  items: StackingItem[]
  destructionVersion: number
  onPlace: (point: Vec3) => void
  onRemove: (id: string) => void
}

interface Blast {
  id: number
  center: Vec3
}

function SceneContent(props: GameSceneProps) {
  const bodies = useRef(new Map<string, RapierRigidBody>())
  const [blast, setBlast] = useState<Blast | null>(null)

  const registerBody = useCallback((id: string, body: RapierRigidBody | null) => {
    if (body) {
      bodies.current.set(id, body)
    } else {
      bodies.current.delete(id)
    }
  }, [])

  const handlePlace = useCallback(
    (event: ThreeEvent<MouseEvent>) => {
      event.stopPropagation()
      if (event.delta > 6) return
      props.onPlace([event.point.x, event.point.y, event.point.z])
    },
    [props.onPlace],
  )

  useEffect(() => {
    if (props.destructionVersion === 0) return
    const activeBodies = [...bodies.current.values()].filter((body) => body.isValid())
    const positions = activeBodies.map((body) => {
      const point = body.translation()
      return [point.x, point.y, point.z] as Vec3
    })
    const center = getExplosionCenter(positions)
    if (!center) return

    activeBodies.forEach((body) => {
      const point = body.translation()
      const impulse = getExplosionImpulse([point.x, point.y, point.z], center)
      body.applyImpulse({ x: impulse[0], y: impulse[1], z: impulse[2] }, true)
      body.applyTorqueImpulse(
        {
          x: (Math.random() - 0.5) * 8,
          y: (Math.random() - 0.5) * 8,
          z: (Math.random() - 0.5) * 8,
        },
        true,
      )
    })
    setBlast({ id: props.destructionVersion, center })
  }, [props.destructionVersion])

  return (
    <>
      <color attach="background" args={['#171125']} />
      <fog attach="fog" args={['#171125', 18, 42]} />
      <Stars radius={60} depth={25} count={700} factor={2} saturation={0.3} fade />
      <ambientLight intensity={0.75} />
      <directionalLight
        castShadow
        position={[8, 14, 7]}
        intensity={2.6}
        color="#ffe3b3"
        shadow-mapSize-width={2048}
        shadow-mapSize-height={2048}
        shadow-camera-far={35}
        shadow-camera-left={-12}
        shadow-camera-right={12}
        shadow-camera-top={12}
        shadow-camera-bottom={-12}
      />
      <pointLight
        position={[-8, 7, -5]}
        intensity={45}
        color="#7848ff"
        distance={25}
      />

      <Suspense fallback={null}>
        <Physics gravity={[0, -9.81, 0]} colliders={false}>
          <RigidBody type="fixed" colliders={false}>
            <CylinderCollider args={[0.6, 7]} position={[0, -0.6, 0]} friction={1} />
            <mesh receiveShadow position={[0, -0.6, 0]} onClick={handlePlace}>
              <cylinderGeometry args={[7, 7, 1.2, 64]} />
              <meshStandardMaterial color="#302942" roughness={0.82} metalness={0.16} />
            </mesh>
            <mesh position={[0, 0.015, 0]} rotation={[-Math.PI / 2, 0, 0]}>
              <ringGeometry args={[6.45, 6.85, 64]} />
              <meshBasicMaterial color="#8e72d8" transparent opacity={0.5} />
            </mesh>
          </RigidBody>

          {props.items.map((item) => (
            <StackingObject
              key={item.id}
              item={item}
              onPlace={handlePlace}
              onRemove={props.onRemove}
              registerBody={registerBody}
            />
          ))}
        </Physics>
      </Suspense>

      {blast && <Shockwave key={blast.id} center={blast.center} />}
      <CameraJolt trigger={props.destructionVersion} />
      <OrbitControls
        makeDefault
        enableDamping
        minDistance={8}
        maxDistance={28}
        minPolarAngle={0.35}
        maxPolarAngle={Math.PI / 2.05}
        target={[0, 2.4, 0]}
      />
    </>
  )
}

export function GameScene(props: GameSceneProps) {
  return (
    <div className="game-canvas" aria-label="3D積み立てゲーム">
      <Canvas
        shadows
        dpr={[1, 1.75]}
        camera={{ position: [12, 10, 14], fov: 46 }}
        gl={{ antialias: true }}
      >
        <SceneContent {...props} />
      </Canvas>
    </div>
  )
}
