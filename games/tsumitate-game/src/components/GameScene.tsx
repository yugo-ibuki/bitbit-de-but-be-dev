import {
  Suspense,
  useCallback,
  useEffect,
  useRef,
  useState,
} from 'react'
import { Canvas, useFrame, type ThreeEvent } from '@react-three/fiber'
import { OrbitControls, Stars } from '@react-three/drei'
import {
  CuboidCollider,
  CylinderCollider,
  Physics,
  RigidBody,
  type RapierRigidBody,
} from '@react-three/rapier'
import {
  createContainmentSegments,
  getExplosionCenter,
  getExplosionImpulse,
} from '../game/gameRules'
import {
  canAccumulateStability,
  getObjectTopY,
  getTowerHeight,
} from '../game/heightMeasurement'
import type { StackingItem, Vec3 } from '../game/types'
import { CameraJolt, Shockwave } from './SceneEffects'
import { ElevatedDropZone } from './ElevatedDropZone'
import { MeasurementGuide } from './MeasurementGuide'
import { StackingObject } from './StackingObject'

interface GameSceneProps {
  items: StackingItem[]
  destructionVersion: number
  containmentEnabled: boolean
  currentHeight: number
  onPlace: (point: Vec3) => void
  onRemove: (id: string) => void
  onHeightChange: (height: number, eligibleForRecord: boolean) => void
}

interface Blast {
  id: number
  center: Vec3
}

const containmentSegments = createContainmentSegments()

function ContainmentWall() {
  return (
    <RigidBody type="fixed" colliders={false}>
      {containmentSegments.map((segment, index) => (
        <CuboidCollider
          key={index}
          args={[0.72, 6, 0.18]}
          position={segment.position}
          rotation={[0, segment.rotationY, 0]}
          friction={0.9}
          restitution={0.12}
        />
      ))}
    </RigidBody>
  )
}

function SceneContent(props: GameSceneProps) {
  const bodies = useRef(new Map<string, RapierRigidBody>())
  const measurementElapsed = useRef(0)
  const stableElapsed = useRef(0)
  const lastMeasuredHeight = useRef(-1)
  const lastRecordableHeight = useRef(-1)
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

  useFrame((_, delta) => {
    if (!props.containmentEnabled) {
      measurementElapsed.current = 0
      stableElapsed.current = 0
      return
    }

    measurementElapsed.current += delta
    if (measurementElapsed.current < 0.1) return
    measurementElapsed.current = 0

    const activeBodies = props.items.flatMap((item) => {
      const body = bodies.current.get(item.id)
      if (!body?.isValid()) return []
      return [{ item, body }]
    })
    const allStable = activeBodies.every(({ body }) => {
      const linear = body.linvel()
      const angular = body.angvel()
      return (
        Math.hypot(linear.x, linear.y, linear.z) < 0.18 &&
        Math.hypot(angular.x, angular.y, angular.z) < 0.25
      )
    })
    stableElapsed.current = canAccumulateStability(
      activeBodies.length,
      props.items.length,
      allStable,
    )
      ? stableElapsed.current + 0.1
      : 0

    const bounds = activeBodies.map(({ item, body }) => {
      const position = body.translation()
      return {
        maxY: getObjectTopY(item.kind, position.y, body.rotation(), item.scale),
      }
    })
    const height = getTowerHeight(bounds)
    const eligibleForRecord = stableElapsed.current >= 0.4
    const heightChanged = height !== lastMeasuredHeight.current
    const recordCandidateChanged =
      eligibleForRecord && height !== lastRecordableHeight.current
    if (!heightChanged && !recordCandidateChanged) return
    lastMeasuredHeight.current = height
    if (eligibleForRecord) lastRecordableHeight.current = height
    props.onHeightChange(height, eligibleForRecord)
  })

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
      <color attach="background" args={['#0b111a']} />
      <fog attach="fog" args={['#0b111a', 18, 42]} />
      <Stars radius={60} depth={25} count={520} factor={2} saturation={0.25} fade />
      <ambientLight intensity={0.68} color="#c8f4ff" />
      <directionalLight
        castShadow
        position={[8, 14, 7]}
        intensity={2.6}
        color="#d7f7ff"
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
        color="#36d8ff"
        distance={25}
      />

      <ElevatedDropZone onPlace={props.onPlace} />

      <Suspense fallback={null}>
        <Physics gravity={[0, -9.81, 0]} colliders={false}>
          <RigidBody type="fixed" colliders={false}>
            <CylinderCollider args={[0.6, 7]} position={[0, -0.6, 0]} friction={1} />
            <mesh
              receiveShadow
              position={[0, -0.6, 0]}
              userData={{ placementSurface: true }}
              onClick={handlePlace}
            >
              <cylinderGeometry args={[7, 7, 1.2, 64]} />
              <meshStandardMaterial color="#172832" roughness={0.72} metalness={0.28} />
            </mesh>
            <mesh position={[0, 0.015, 0]} rotation={[-Math.PI / 2, 0, 0]}>
              <ringGeometry args={[6.45, 6.85, 64]} />
              <meshBasicMaterial color="#69e6ff" transparent opacity={0.62} />
            </mesh>
          </RigidBody>

          {props.containmentEnabled && <ContainmentWall />}

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

      <Suspense fallback={null}>
        <MeasurementGuide height={props.currentHeight} />
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
