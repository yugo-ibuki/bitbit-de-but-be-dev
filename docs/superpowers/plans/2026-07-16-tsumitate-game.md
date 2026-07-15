# Tsumitate Game Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a browser-playable 3D stacking game where players drop three kinds of objects, pile them up with physics, explode the pile, and reset to play again.

**Architecture:** A Vite React app owns serializable game state in a reducer. React Three Fiber renders the scene, React Three Rapier owns rigid bodies, and ordinary React components render the HUD; deterministic rules stay in pure functions covered by Vitest.

**Tech Stack:** React 19, TypeScript 7, Vite 8, Three.js, React Three Fiber 9, Drei 10, React Three Rapier 2, Vitest 4, React Testing Library

---

## File map

- `games/tsumitate-game/package.json` — independent project scripts and pinned dependencies
- `games/tsumitate-game/index.html` — Vite entry document
- `games/tsumitate-game/tsconfig.json` — strict TypeScript configuration
- `games/tsumitate-game/vite.config.ts` — React and Vitest configuration
- `games/tsumitate-game/src/main.tsx` — React root
- `games/tsumitate-game/src/App.tsx` — reducer wiring and top-level composition
- `games/tsumitate-game/src/styles.css` — responsive HUD and full-screen presentation
- `games/tsumitate-game/src/game/types.ts` — shared serializable types
- `games/tsumitate-game/src/game/gameRules.ts` — shape, spawn, bounds, and explosion math
- `games/tsumitate-game/src/game/gameRules.test.ts` — pure rule tests
- `games/tsumitate-game/src/game/gameState.ts` — reducer and actions
- `games/tsumitate-game/src/game/gameState.test.ts` — reducer tests
- `games/tsumitate-game/src/components/GameHud.tsx` — accessible 2D controls
- `games/tsumitate-game/src/components/GameHud.test.tsx` — HUD behavior tests
- `games/tsumitate-game/src/components/GameScene.tsx` — Canvas, stage, physics world, placement, destruction
- `games/tsumitate-game/src/components/StackingObject.tsx` — one dynamic shape and rigid body
- `games/tsumitate-game/src/components/SceneEffects.tsx` — shockwave and short camera jolt
- `games/tsumitate-game/src/components/GameErrorBoundary.tsx` — runtime failure fallback
- `games/tsumitate-game/src/components/GameErrorBoundary.test.tsx` — fallback test
- `games/tsumitate-game/src/test/setup.ts` — Testing Library matchers
- `games/tsumitate-game/README.md` — run commands and controls

### Task 1: Scaffold the independent React project

**Files:**
- Create: `games/tsumitate-game/package.json`
- Create: `games/tsumitate-game/tsconfig.json`
- Create: `games/tsumitate-game/vite.config.ts`
- Create: `games/tsumitate-game/index.html`
- Create: `games/tsumitate-game/src/main.tsx`
- Create: `games/tsumitate-game/src/App.tsx`
- Create: `games/tsumitate-game/src/test/setup.ts`

- [ ] **Step 1: Create the package manifest**

```json
{
  "name": "tsumitate-game",
  "private": true,
  "version": "0.1.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc --noEmit && vite build",
    "typecheck": "tsc --noEmit",
    "test": "vitest run",
    "test:watch": "vitest"
  },
  "dependencies": {
    "@react-three/drei": "10.7.7",
    "@react-three/fiber": "9.6.1",
    "@react-three/rapier": "2.2.0",
    "react": "19.2.7",
    "react-dom": "19.2.7",
    "three": "0.185.1"
  },
  "devDependencies": {
    "@testing-library/jest-dom": "6.9.1",
    "@testing-library/react": "16.3.2",
    "@testing-library/user-event": "14.6.1",
    "@types/react": "19.2.17",
    "@types/react-dom": "19.2.3",
    "@types/three": "0.185.1",
    "@vitejs/plugin-react": "6.0.3",
    "jsdom": "29.1.1",
    "typescript": "7.0.2",
    "vite": "8.1.4",
    "vitest": "4.1.10"
  }
}
```

- [ ] **Step 2: Create TypeScript and Vite configuration**

`games/tsumitate-game/tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "useDefineForClassFields": true,
    "lib": ["ES2022", "DOM", "DOM.Iterable"],
    "allowJs": false,
    "skipLibCheck": true,
    "esModuleInterop": true,
    "allowSyntheticDefaultImports": true,
    "strict": true,
    "forceConsistentCasingInFileNames": true,
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",
    "types": ["vitest/globals", "@testing-library/jest-dom"]
  },
  "include": ["src", "vite.config.ts"]
}
```

`games/tsumitate-game/vite.config.ts`:

```ts
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [react()],
  test: {
    environment: 'jsdom',
    setupFiles: './src/test/setup.ts',
    css: true,
  },
})
```

- [ ] **Step 3: Create the HTML and minimal React entry**

`games/tsumitate-game/index.html`:

```html
<!doctype html>
<html lang="ja">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="theme-color" content="#151025" />
    <title>積み立てクラッシュ</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.tsx"></script>
  </body>
</html>
```

`games/tsumitate-game/src/main.tsx`:

```tsx
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { App } from './App'

const root = document.getElementById('root')

if (!root) {
  throw new Error('React root element was not found')
}

createRoot(root).render(
  <StrictMode>
    <App />
  </StrictMode>,
)
```

`games/tsumitate-game/src/App.tsx`:

```tsx
export function App() {
  return <main>積み立てクラッシュ</main>
}
```

`games/tsumitate-game/src/test/setup.ts`:

```ts
import '@testing-library/jest-dom/vitest'
```

- [ ] **Step 4: Install dependencies and verify the scaffold**

Run: `cd games/tsumitate-game && npm install && npm run typecheck && npm run build`
Expected: dependency installation succeeds, TypeScript exits 0, and Vite creates `dist/`.

- [ ] **Step 5: Commit the scaffold**

```bash
git add games/tsumitate-game
git commit -m "feat: scaffold tsumitate game"
```

### Task 2: Implement game rules with TDD

**Files:**
- Create: `games/tsumitate-game/src/game/types.ts`
- Create: `games/tsumitate-game/src/game/gameRules.ts`
- Create: `games/tsumitate-game/src/game/gameRules.test.ts`

- [ ] **Step 1: Define the shared types**

```ts
export type ShapeKind = 'box' | 'sphere' | 'cylinder'
export type Vec3 = readonly [number, number, number]

export interface StackingItem {
  id: string
  kind: ShapeKind
  position: Vec3
  rotation: Vec3
  color: string
}
```

- [ ] **Step 2: Write failing rule tests**

```ts
import { describe, expect, it } from 'vitest'
import {
  FALL_LIMIT_Y,
  MAX_OBJECTS,
  canSpawn,
  createStackingItem,
  getExplosionCenter,
  getExplosionImpulse,
  isOutOfBounds,
} from './gameRules'

describe('gameRules', () => {
  it('blocks spawning at the object limit', () => {
    expect(canSpawn(MAX_OBJECTS - 1)).toBe(true)
    expect(canSpawn(MAX_OBJECTS)).toBe(false)
  })

  it('creates an item above the selected point', () => {
    const random = () => 0.5
    const item = createStackingItem('sphere', [1, 2, 3], 'item-1', random)
    expect(item.position).toEqual([1, 5, 3])
    expect(item.rotation).toEqual([0, 0, 0])
  })

  it('calculates the average explosion center', () => {
    expect(getExplosionCenter([[0, 0, 0], [2, 4, 6]])).toEqual([1, 2, 3])
    expect(getExplosionCenter([])).toBeNull()
  })

  it('pushes away from the center with an upward component', () => {
    const impulse = getExplosionImpulse([5, 0, 0], [0, 0, 0])
    expect(impulse[0]).toBeGreaterThan(0)
    expect(impulse[1]).toBeGreaterThan(0)
    expect(impulse[2]).toBeCloseTo(0)
  })

  it('detects objects below the fall limit', () => {
    expect(isOutOfBounds(FALL_LIMIT_Y - 0.01)).toBe(true)
    expect(isOutOfBounds(FALL_LIMIT_Y)).toBe(false)
  })
})
```

- [ ] **Step 3: Run the tests to prove they fail**

Run: `cd games/tsumitate-game && npm test -- src/game/gameRules.test.ts`
Expected: FAIL because `./gameRules` does not exist.

- [ ] **Step 4: Implement the minimal rules**

```ts
import type { ShapeKind, StackingItem, Vec3 } from './types'

export const MAX_OBJECTS = 100
export const FALL_LIMIT_Y = -20

const COLORS = ['#ff6b6b', '#ffd166', '#06d6a0', '#4cc9f0', '#b517ff'] as const

export const SHAPE_CONFIG: Record<
  ShapeKind,
  { label: string; dropOffset: number; collider: 'cuboid' | 'ball' | 'hull' }
> = {
  box: { label: 'ボックス', dropOffset: 3.4, collider: 'cuboid' },
  sphere: { label: 'ボール', dropOffset: 3, collider: 'ball' },
  cylinder: { label: 'シリンダー', dropOffset: 3.3, collider: 'hull' },
}

export function canSpawn(count: number): boolean {
  return count < MAX_OBJECTS
}

export function createStackingItem(
  kind: ShapeKind,
  point: Vec3,
  id: string,
  random: () => number = Math.random,
): StackingItem {
  const angle = () => (random() - 0.5) * 0.36
  const colorIndex = Math.min(COLORS.length - 1, Math.floor(random() * COLORS.length))

  return {
    id,
    kind,
    position: [point[0], point[1] + SHAPE_CONFIG[kind].dropOffset, point[2]],
    rotation: [angle(), angle(), angle()],
    color: COLORS[colorIndex],
  }
}

export function getExplosionCenter(points: readonly Vec3[]): Vec3 | null {
  if (points.length === 0) return null
  const sum = points.reduce(
    (total, point) => [total[0] + point[0], total[1] + point[1], total[2] + point[2]] as Vec3,
    [0, 0, 0] as Vec3,
  )
  return [sum[0] / points.length, sum[1] / points.length, sum[2] / points.length]
}

export function getExplosionImpulse(position: Vec3, center: Vec3): Vec3 {
  let dx = position[0] - center[0]
  const dy = position[1] - center[1] + 0.8
  let dz = position[2] - center[2]
  if (Math.abs(dx) + Math.abs(dz) < 0.001) {
    dx = 0.24
    dz = 0.16
  }
  const length = Math.hypot(dx, dy, dz)
  const distance = Math.hypot(position[0] - center[0], position[1] - center[1], position[2] - center[2])
  const strength = Math.max(9, 30 - distance * 2.2)
  return [(dx / length) * strength, (dy / length) * strength, (dz / length) * strength]
}

export function isOutOfBounds(y: number): boolean {
  return y < FALL_LIMIT_Y
}
```

- [ ] **Step 5: Run the rule tests**

Run: `cd games/tsumitate-game && npm test -- src/game/gameRules.test.ts`
Expected: 5 tests PASS.

- [ ] **Step 6: Commit the rules**

```bash
git add games/tsumitate-game/src/game
git commit -m "feat: add stacking game rules"
```

### Task 3: Build the HUD with TDD

**Files:**
- Create: `games/tsumitate-game/src/components/GameHud.tsx`
- Create: `games/tsumitate-game/src/components/GameHud.test.tsx`

- [ ] **Step 1: Write the failing HUD test**

```tsx
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { describe, expect, it, vi } from 'vitest'
import { GameHud } from './GameHud'

describe('GameHud', () => {
  it('shows state and sends player actions', async () => {
    const user = userEvent.setup()
    const onSelect = vi.fn()
    const onDestroy = vi.fn()
    const onReset = vi.fn()

    render(
      <GameHud
        selectedKind="box"
        count={12}
        max={100}
        notice="もっと積めます"
        onSelect={onSelect}
        onDestroy={onDestroy}
        onReset={onReset}
      />,
    )

    expect(screen.getByText('12 / 100')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'ボックス' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText('もっと積めます')).toBeInTheDocument()

    await user.click(screen.getByRole('button', { name: 'ボール' }))
    await user.click(screen.getByRole('button', { name: '破壊する' }))
    await user.click(screen.getByRole('button', { name: 'もう一度積む' }))

    expect(onSelect).toHaveBeenCalledWith('sphere')
    expect(onDestroy).toHaveBeenCalledOnce()
    expect(onReset).toHaveBeenCalledOnce()
  })
})
```

- [ ] **Step 2: Run the HUD test to prove it fails**

Run: `cd games/tsumitate-game && npm test -- src/components/GameHud.test.tsx`
Expected: FAIL because `GameHud` does not exist.

- [ ] **Step 3: Implement the HUD**

```tsx
import type { ShapeKind } from '../game/types'
import { SHAPE_CONFIG } from '../game/gameRules'

interface GameHudProps {
  selectedKind: ShapeKind
  count: number
  max: number
  notice: string | null
  onSelect: (kind: ShapeKind) => void
  onDestroy: () => void
  onReset: () => void
}

const kinds: ShapeKind[] = ['box', 'sphere', 'cylinder']

export function GameHud(props: GameHudProps) {
  return (
    <div className="hud">
      <header className="brand">
        <p className="eyebrow">STACK · WATCH · CRASH</p>
        <h1>積み立てクラッシュ</h1>
        <p>好きな形を落として、思いきり崩そう。</p>
      </header>

      <section className="shape-panel" aria-label="落とす形">
        {kinds.map((kind) => (
          <button
            className="shape-button"
            type="button"
            key={kind}
            aria-pressed={props.selectedKind === kind}
            onClick={() => props.onSelect(kind)}
          >
            <span className={'shape-icon ' + kind} aria-hidden="true" />
            {SHAPE_CONFIG[kind].label}
          </button>
        ))}
      </section>

      <aside className="counter" aria-label="物体数">
        <span>OBJECTS</span>
        <strong>{props.count} / {props.max}</strong>
      </aside>

      <p className="instructions">タップで投下 · ドラッグで回転 · ホイールでズーム</p>
      {props.notice && <p className="notice" role="status">{props.notice}</p>}

      <div className="actions">
        <button className="reset-button" type="button" onClick={props.onReset}>
          もう一度積む
        </button>
        <button className="destroy-button" type="button" onClick={props.onDestroy}>
          <span aria-hidden="true">✦</span> 破壊する
        </button>
      </div>
    </div>
  )
}
```

- [ ] **Step 4: Run the HUD test**

Run: `cd games/tsumitate-game && npm test -- src/components/GameHud.test.tsx`
Expected: 1 test PASS.

- [ ] **Step 5: Commit the HUD**

```bash
git add games/tsumitate-game/src/components/GameHud.tsx games/tsumitate-game/src/components/GameHud.test.tsx
git commit -m "feat: add stacking game HUD"
```

### Task 4: Implement reducer-driven game state with TDD

**Files:**
- Create: `games/tsumitate-game/src/game/gameState.ts`
- Create: `games/tsumitate-game/src/game/gameState.test.ts`

- [ ] **Step 1: Write reducer tests**

```ts
import { describe, expect, it } from 'vitest'
import { createStackingItem } from './gameRules'
import { initialGameState, gameReducer } from './gameState'

describe('gameReducer', () => {
  it('selects a shape and adds an item', () => {
    const selected = gameReducer(initialGameState, { type: 'select', kind: 'sphere' })
    const item = createStackingItem('sphere', [0, 0, 0], 'item-1', () => 0.5)
    const placed = gameReducer(selected, { type: 'place', item })
    expect(placed.selectedKind).toBe('sphere')
    expect(placed.items).toEqual([item])
  })

  it('triggers destruction only when objects exist', () => {
    expect(gameReducer(initialGameState, { type: 'destroy' }).notice).toBe('先に物体を積んでください')
    const item = createStackingItem('box', [0, 0, 0], 'item-1', () => 0.5)
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    expect(gameReducer(withItem, { type: 'destroy' }).destructionVersion).toBe(1)
  })

  it('removes fallen objects and resets all objects', () => {
    const item = createStackingItem('box', [0, 0, 0], 'item-1', () => 0.5)
    const withItem = gameReducer(initialGameState, { type: 'place', item })
    expect(gameReducer(withItem, { type: 'remove', id: item.id }).items).toEqual([])
    expect(gameReducer(withItem, { type: 'reset' }).items).toEqual([])
  })
})
```

- [ ] **Step 2: Run the reducer tests to prove they fail**

Run: `cd games/tsumitate-game && npm test -- src/game/gameState.test.ts`
Expected: FAIL because `gameState` does not exist.

- [ ] **Step 3: Implement the reducer**

```ts
import { MAX_OBJECTS, canSpawn } from './gameRules'
import type { ShapeKind, StackingItem } from './types'

export interface GameState {
  selectedKind: ShapeKind
  items: StackingItem[]
  destructionVersion: number
  notice: string | null
}

export type GameAction =
  | { type: 'select'; kind: ShapeKind }
  | { type: 'place'; item: StackingItem }
  | { type: 'remove'; id: string }
  | { type: 'destroy' }
  | { type: 'reset' }
  | { type: 'clear-notice' }

export const initialGameState: GameState = {
  selectedKind: 'box',
  items: [],
  destructionVersion: 0,
  notice: null,
}

export function gameReducer(state: GameState, action: GameAction): GameState {
  switch (action.type) {
    case 'select':
      return { ...state, selectedKind: action.kind, notice: null }
    case 'place':
      if (!canSpawn(state.items.length)) {
        return { ...state, notice: '物体は' + MAX_OBJECTS + '個までです。そろそろ壊しましょう！' }
      }
      return { ...state, items: [...state.items, action.item], notice: null }
    case 'remove':
      return { ...state, items: state.items.filter((item) => item.id !== action.id) }
    case 'destroy':
      if (state.items.length === 0) {
        return { ...state, notice: '先に物体を積んでください' }
      }
      return { ...state, destructionVersion: state.destructionVersion + 1, notice: null }
    case 'reset':
      return { ...state, items: [], notice: null }
    case 'clear-notice':
      return { ...state, notice: null }
  }
}
```

- [ ] **Step 4: Run all unit tests**

Run: `cd games/tsumitate-game && npm test`
Expected: rule, HUD, and reducer suites all PASS.

- [ ] **Step 5: Commit reducer state**

```bash
git add games/tsumitate-game/src/game/gameState.ts games/tsumitate-game/src/game/gameState.test.ts
git commit -m "feat: add stacking game state"
```

### Task 5: Build the physical scene and placement

**Files:**
- Create: `games/tsumitate-game/src/components/StackingObject.tsx`
- Create: `games/tsumitate-game/src/components/SceneEffects.tsx`
- Create: `games/tsumitate-game/src/components/GameScene.tsx`

- [ ] **Step 1: Implement one dynamic stacking object**

```tsx
import { useEffect, useRef } from 'react'
import type { ThreeEvent } from '@react-three/fiber'
import { useFrame } from '@react-three/fiber'
import { RigidBody, type RapierRigidBody } from '@react-three/rapier'
import { SHAPE_CONFIG, isOutOfBounds } from '../game/gameRules'
import type { StackingItem } from '../game/types'

interface StackingObjectProps {
  item: StackingItem
  onPlace: (event: ThreeEvent<MouseEvent>) => void
  onRemove: (id: string) => void
  registerBody: (id: string, body: RapierRigidBody | null) => void
}

export function StackingObject({ item, onPlace, onRemove, registerBody }: StackingObjectProps) {
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
      <mesh castShadow receiveShadow onClick={onPlace}>
        {item.kind === 'box' && <boxGeometry args={[1.7, 1.2, 1.5]} />}
        {item.kind === 'sphere' && <sphereGeometry args={[0.9, 32, 24]} />}
        {item.kind === 'cylinder' && <cylinderGeometry args={[0.82, 0.82, 1.8, 32]} />}
        <meshStandardMaterial color={item.color} roughness={0.46} metalness={0.08} />
      </mesh>
    </RigidBody>
  )
}
```

- [ ] **Step 2: Implement the shockwave and camera jolt**

```tsx
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
    if (material.current) material.current.opacity = (1 - progress) * 0.85
  })

  return (
    <mesh ref={mesh} position={[center[0], Math.max(0.08, center[1]), center[2]]} rotation={[-Math.PI / 2, 0, 0]}>
      <ringGeometry args={[0.75, 1, 64]} />
      <meshBasicMaterial ref={material} color="#ffcf5c" transparent depthWrite={false} />
    </mesh>
  )
}

export function CameraJolt({ trigger }: { trigger: number }) {
  const camera = useThree((state) => state.camera)
  const remaining = useRef(0)
  const previous = useRef({ x: 0, y: 0, z: 0 })

  useEffect(() => {
    if (trigger > 0) remaining.current = 0.42
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
```

- [ ] **Step 3: Implement the stage, physics world, placement, and destruction**

```tsx
import { Suspense, useCallback, useEffect, useRef, useState } from 'react'
import { Canvas, type ThreeEvent } from '@react-three/fiber'
import { OrbitControls } from '@react-three/drei'
import {
  CylinderCollider,
  Physics,
  RigidBody,
  type RapierRigidBody,
} from '@react-three/rapier'
import {
  getExplosionCenter,
  getExplosionImpulse,
} from '../game/gameRules'
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
    if (body) bodies.current.set(id, body)
    else bodies.current.delete(id)
  }, [])

  const handlePlace = useCallback((event: ThreeEvent<MouseEvent>) => {
    event.stopPropagation()
    if (event.delta > 6) return
    props.onPlace([event.point.x, event.point.y, event.point.z])
  }, [props.onPlace])

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
        { x: (Math.random() - 0.5) * 8, y: (Math.random() - 0.5) * 8, z: (Math.random() - 0.5) * 8 },
        true,
      )
    })
    setBlast({ id: props.destructionVersion, center })
  }, [props.destructionVersion])

  return (
    <>
      <color attach="background" args={['#171125']} />
      <fog attach="fog" args={['#171125', 18, 42]} />
      <ambientLight intensity={0.75} />
      <directionalLight
        castShadow
        position={[8, 14, 7]}
        intensity={2.6}
        color="#ffe3b3"
        shadow-mapSize-width={2048}
        shadow-mapSize-height={2048}
      />
      <pointLight position={[-8, 7, -5]} intensity={45} color="#7848ff" distance={25} />

      <Suspense fallback={null}>
        <Physics gravity={[0, -9.81, 0]} colliders={false}>
          <RigidBody type="fixed" colliders={false}>
            <CylinderCollider args={[0.6, 7]} position={[0, -0.6, 0]} friction={1} />
            <mesh receiveShadow position={[0, -0.6, 0]} onClick={handlePlace}>
              <cylinderGeometry args={[7, 7, 1.2, 64]} />
              <meshStandardMaterial color="#302942" roughness={0.82} metalness={0.16} />
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
      <Canvas shadows dpr={[1, 1.75]} camera={{ position: [12, 10, 14], fov: 46 }}>
        <SceneContent {...props} />
      </Canvas>
    </div>
  )
}
```

- [ ] **Step 4: Typecheck the scene**

Run: `cd games/tsumitate-game && npm run typecheck`
Expected: PASS with no TypeScript diagnostics.

- [ ] **Step 5: Commit the physical scene**

```bash
git add games/tsumitate-game/src/components/StackingObject.tsx games/tsumitate-game/src/components/SceneEffects.tsx games/tsumitate-game/src/components/GameScene.tsx
git commit -m "feat: add 3D stacking physics"
```

### Task 6: Integrate the game and visual design

**Files:**
- Modify: `games/tsumitate-game/src/App.tsx`
- Modify: `games/tsumitate-game/src/main.tsx`
- Create: `games/tsumitate-game/src/styles.css`

- [ ] **Step 1: Replace the scaffold App with reducer wiring**

```tsx
import { useCallback, useEffect, useReducer, useRef } from 'react'
import { GameHud } from './components/GameHud'
import { GameScene } from './components/GameScene'
import { MAX_OBJECTS, createStackingItem } from './game/gameRules'
import { gameReducer, initialGameState } from './game/gameState'
import type { Vec3 } from './game/types'

export function App() {
  const [state, dispatch] = useReducer(gameReducer, initialGameState)
  const nextId = useRef(1)

  const placeObject = useCallback((point: Vec3) => {
    const item = createStackingItem(
      state.selectedKind,
      point,
      'object-' + nextId.current++,
    )
    dispatch({ type: 'place', item })
  }, [state.selectedKind])

  useEffect(() => {
    if (!state.notice) return
    const timer = window.setTimeout(() => dispatch({ type: 'clear-notice' }), 2600)
    return () => window.clearTimeout(timer)
  }, [state.notice])

  return (
    <main className="app-shell">
      <GameScene
        items={state.items}
        destructionVersion={state.destructionVersion}
        onPlace={placeObject}
        onRemove={(id) => dispatch({ type: 'remove', id })}
      />
      <GameHud
        selectedKind={state.selectedKind}
        count={state.items.length}
        max={MAX_OBJECTS}
        notice={state.notice}
        onSelect={(kind) => dispatch({ type: 'select', kind })}
        onDestroy={() => dispatch({ type: 'destroy' })}
        onReset={() => dispatch({ type: 'reset' })}
      />
    </main>
  )
}
```

- [ ] **Step 2: Import the stylesheet**

Add to `src/main.tsx` after the App import:

```ts
import './styles.css'
```

- [ ] **Step 3: Add the complete responsive visual styling**

```css
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;600;700&family=Noto+Sans+JP:wght@500;700;900&display=swap');

:root {
  font-family: 'DM Sans', 'Noto Sans JP', sans-serif;
  color: #fff9ee;
  background: #171125;
  font-synthesis: none;
  text-rendering: optimizeLegibility;
}

* { box-sizing: border-box; }
html, body, #root { width: 100%; height: 100%; margin: 0; overflow: hidden; }
button { font: inherit; }
.app-shell { position: relative; width: 100%; height: 100%; background: radial-gradient(circle at 50% 45%, #322253 0%, #171125 60%); }
.game-canvas { position: absolute; inset: 0; }
.hud { position: absolute; inset: 0; pointer-events: none; padding: clamp(18px, 3vw, 42px); }
.hud button { pointer-events: auto; }
.brand { max-width: 430px; text-shadow: 0 3px 18px #08050d; }
.brand h1 { margin: 2px 0 8px; font-family: 'Noto Sans JP', sans-serif; font-size: clamp(28px, 4vw, 54px); line-height: 1.08; letter-spacing: -0.06em; }
.brand p { margin: 0; color: #d9cee8; font-weight: 600; }
.brand .eyebrow { color: #ffca66; font-size: 12px; letter-spacing: 0.2em; }
.shape-panel { position: absolute; top: clamp(18px, 3vw, 42px); right: clamp(18px, 3vw, 42px); display: flex; gap: 8px; padding: 8px; border: 1px solid #ffffff26; border-radius: 18px; background: #1d162eb8; backdrop-filter: blur(14px); }
.shape-button { min-width: 88px; padding: 10px 12px; border: 0; border-radius: 12px; color: #d9cee8; background: transparent; cursor: pointer; transition: 160ms ease; }
.shape-button:hover { background: #ffffff12; color: white; }
.shape-button[aria-pressed='true'] { color: #20162a; background: #ffd06f; box-shadow: 0 7px 24px #ffb84d44; }
.shape-icon { display: block; width: 22px; height: 22px; margin: 0 auto 5px; background: currentColor; }
.shape-icon.box { border-radius: 4px; }
.shape-icon.sphere { border-radius: 50%; }
.shape-icon.cylinder { border-radius: 50% / 22%; }
.counter { position: absolute; right: clamp(18px, 3vw, 42px); top: 126px; display: grid; text-align: right; color: #c9bdd8; font-size: 11px; letter-spacing: 0.15em; }
.counter strong { color: white; font-size: 22px; letter-spacing: 0; }
.instructions { position: absolute; left: 50%; bottom: 28px; transform: translateX(-50%); margin: 0; padding: 9px 14px; border-radius: 999px; color: #cabfd8; background: #130e20a8; font-size: 12px; white-space: nowrap; }
.actions { position: absolute; right: clamp(18px, 3vw, 42px); bottom: clamp(18px, 3vw, 42px); display: flex; gap: 10px; }
.actions button { border: 0; border-radius: 999px; padding: 14px 20px; color: white; cursor: pointer; font-weight: 800; box-shadow: 0 10px 30px #09050e66; }
.reset-button { background: #393044; }
.destroy-button { min-width: 150px; background: linear-gradient(135deg, #ff4d67, #ff9b42); transform: translateZ(0); transition: 150ms ease; }
.destroy-button:hover { transform: scale(1.04); box-shadow: 0 12px 35px #ff5d4f55; }
.notice { position: absolute; left: 50%; top: 18%; transform: translateX(-50%); margin: 0; padding: 12px 18px; border: 1px solid #ffd06f66; border-radius: 12px; color: #22172c; background: #ffd06f; font-weight: 800; box-shadow: 0 12px 40px #08050d77; }

@media (max-width: 720px) {
  .hud { padding: 16px; }
  .brand h1 { font-size: 29px; }
  .brand > p:last-child { display: none; }
  .shape-panel { top: auto; right: 12px; bottom: 88px; left: 12px; justify-content: center; }
  .shape-button { flex: 1; min-width: 0; padding: 8px 4px; font-size: 12px; }
  .counter { top: 18px; right: 16px; }
  .actions { right: 12px; bottom: 14px; left: 12px; }
  .actions button { flex: 1; padding: 13px 10px; }
  .instructions { bottom: 152px; max-width: calc(100vw - 24px); overflow: hidden; text-overflow: ellipsis; }
}
```

- [ ] **Step 4: Run tests, typecheck, and build**

Run: `cd games/tsumitate-game && npm test && npm run typecheck && npm run build`
Expected: all test suites PASS, typecheck exits 0, and Vite creates `dist/`.

- [ ] **Step 5: Commit integration and styling**

```bash
git add games/tsumitate-game/src/App.tsx games/tsumitate-game/src/main.tsx games/tsumitate-game/src/styles.css
git commit -m "feat: integrate playable stacking game"
```

### Task 7: Add runtime fallback and project documentation

**Files:**
- Create: `games/tsumitate-game/src/components/GameErrorBoundary.tsx`
- Create: `games/tsumitate-game/src/components/GameErrorBoundary.test.tsx`
- Modify: `games/tsumitate-game/src/App.tsx`
- Create: `games/tsumitate-game/README.md`

- [ ] **Step 1: Write the failing error fallback test**

```tsx
import { render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { GameErrorBoundary } from './GameErrorBoundary'

function BrokenScene(): never {
  throw new Error('WebGL unavailable')
}

describe('GameErrorBoundary', () => {
  it('shows a useful recovery message', () => {
    vi.spyOn(console, 'error').mockImplementation(() => undefined)
    render(
      <GameErrorBoundary>
        <BrokenScene />
      </GameErrorBoundary>,
    )
    expect(screen.getByRole('alert')).toHaveTextContent('ゲームを起動できませんでした')
  })
})
```

- [ ] **Step 2: Run the fallback test to prove it fails**

Run: `cd games/tsumitate-game && npm test -- src/components/GameErrorBoundary.test.tsx`
Expected: FAIL because `GameErrorBoundary` does not exist.

- [ ] **Step 3: Implement the error boundary**

```tsx
import { Component, type ErrorInfo, type ReactNode } from 'react'

interface Props { children: ReactNode }
interface State { failed: boolean }

export class GameErrorBoundary extends Component<Props, State> {
  state: State = { failed: false }

  static getDerivedStateFromError(): State {
    return { failed: true }
  }

  componentDidCatch(error: Error, info: ErrorInfo) {
    console.error('The 3D game failed to start', error, info)
  }

  render() {
    if (this.state.failed) {
      return (
        <div className="fatal-error" role="alert">
          <h2>ゲームを起動できませんでした</h2>
          <p>WebGL対応ブラウザでページを再読み込みしてください。</p>
          <button type="button" onClick={() => window.location.reload()}>再読み込み</button>
        </div>
      )
    }
    return this.props.children
  }
}
```

- [ ] **Step 4: Wrap `GameScene` in `App.tsx`**

Import `GameErrorBoundary` and replace the direct scene element with:

```tsx
<GameErrorBoundary>
  <GameScene
    items={state.items}
    destructionVersion={state.destructionVersion}
    onPlace={placeObject}
    onRemove={(id) => dispatch({ type: 'remove', id })}
  />
</GameErrorBoundary>
```

Append this style to `styles.css`:

```css
.fatal-error { position: absolute; inset: 0; z-index: 10; display: grid; place-content: center; padding: 28px; text-align: center; background: #171125; }
.fatal-error button { justify-self: center; border: 0; border-radius: 999px; padding: 12px 18px; color: #22172c; background: #ffd06f; font-weight: 800; cursor: pointer; }
```

- [ ] **Step 5: Write the README**

````md
# 積み立てクラッシュ

3D空間にボックス、ボール、シリンダーを積み上げ、最後に爆発させて崩壊を眺めるブラウザゲームです。

## 起動

```bash
npm install
npm run dev
```

表示されたローカルURLをブラウザで開きます。

## 操作

- ボックス／ボール／シリンダー: 次に落とす物体を選択
- 舞台または物体をクリック／タップ: その位置の上から投下
- ドラッグ: カメラ回転
- ホイール／ピンチ: ズーム
- 破壊する: 積み上げた物体へ爆発インパルスを付与
- もう一度積む: 動的物体をすべて削除

## 検証

```bash
npm test
npm run typecheck
npm run build
```
````

- [ ] **Step 6: Run the complete automated verification**

Run: `cd games/tsumitate-game && npm test && npm run typecheck && npm run build`
Expected: all tests PASS, no TypeScript diagnostics, production build succeeds.

- [ ] **Step 7: Commit fallback and documentation**

```bash
git add games/tsumitate-game/src/components/GameErrorBoundary.tsx games/tsumitate-game/src/components/GameErrorBoundary.test.tsx games/tsumitate-game/src/App.tsx games/tsumitate-game/src/styles.css games/tsumitate-game/README.md
git commit -m "docs: add game recovery and usage guide"
```

### Task 8: Verify the real browser experience

**Files:**
- Modify only files whose behavior fails the checks below

- [ ] **Step 1: Start the production-equivalent preview**

Run: `cd games/tsumitate-game && npm run build && npm run dev -- --host 127.0.0.1`
Expected: Vite prints a local URL and the page loads without console errors.

- [ ] **Step 2: Verify stacking in a desktop viewport**

At 1440×900, place at least three boxes, three balls, and three cylinders.
Expected: all shapes fall under gravity, collide with the stage and each other, remain visible, and the counter reads `9 / 100`.

- [ ] **Step 3: Verify camera controls do not create accidental objects**

Drag across the scene and use the wheel to zoom.
Expected: the view rotates and zooms; a drag longer than six pixels does not add an object.

- [ ] **Step 4: Verify destruction and repeat play**

Build a pile of at least 15 objects, click `破壊する`, wait three seconds, then click `もう一度積む`.
Expected: the pile receives outward and upward impulses, the shockwave appears, the camera jolts briefly, objects scatter and fall, then reset removes every object and the counter returns to `0 / 100`.

- [ ] **Step 5: Verify responsive controls**

At 390×844, select each shape and place at least one object.
Expected: shape buttons and both action buttons remain fully visible, tapping works, and the HUD does not block the center of the stage.

- [ ] **Step 6: Run the completion audit**

Run:

```bash
cd games/tsumitate-game
npm test
npm run typecheck
npm run build
git diff --check
git status --short
```

Expected: tests, typecheck, and build pass; `git diff --check` prints nothing; only intentional game files are modified or the tree is clean.

- [ ] **Step 7: Commit any browser-found corrections**

If Step 2–5 required corrections:

```bash
git add games/tsumitate-game
git commit -m "fix: polish stacking game interactions"
```

If no files changed, skip this commit.
