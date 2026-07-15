# Lab Measurement UI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an accurate live tower-height readout, persistent personal best, and laboratory-style 3D measurement decoration without disrupting the existing stacking and destruction controls.

**Architecture:** Rapier collider AABBs are sampled inside the 3D scene and reduced to a rounded height above the platform. `App` owns the current and best values, gates record updates to the stacking phase, persists the best through a safe storage helper, and passes the same current value to the HUD and scene guide.

**Tech Stack:** React 19, TypeScript, React Three Fiber, Drei, Rapier, Vitest, Testing Library, CSS

---

## File structure

- Create `src/game/heightMeasurement.ts`: pure height normalization and AABB reduction.
- Create `src/game/heightMeasurement.test.ts`: unit coverage for empty, negative, maximum, and rounding behavior.
- Create `src/game/bestHeightStorage.ts`: guarded localStorage read/write.
- Create `src/game/bestHeightStorage.test.ts`: valid, corrupt, invalid, and throwing storage coverage.
- Create `src/components/MeasurementGuide.tsx`: grid, particles, ruler, and live height marker.
- Modify `src/components/GameScene.tsx`: sample active colliders and notify height changes.
- Modify `src/App.tsx`: own current/best state, persistence, destruction freeze, and reset behavior.
- Modify `src/components/GameHud.tsx`: render the measurement panel.
- Modify `src/components/GameHud.test.tsx`: verify metric formatting and existing actions.
- Modify `src/styles.css`: laboratory palette and responsive metric panel.

### Task 1: Pure height measurement

**Files:**
- Create: `src/game/heightMeasurement.ts`
- Create: `src/game/heightMeasurement.test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import { describe, expect, it } from 'vitest'
import { getTowerHeight } from './heightMeasurement'

describe('getTowerHeight', () => {
  it('returns the highest collider top rounded to centimeters', () => {
    expect(getTowerHeight([{ maxY: 2.346 }, { maxY: 4.204 }])).toBe(4.2)
  })

  it('clamps empty and below-platform bounds to zero', () => {
    expect(getTowerHeight([])).toBe(0)
    expect(getTowerHeight([{ maxY: -2 }])).toBe(0)
  })
})
```

- [ ] **Step 2: Run the test and confirm it fails because the module is missing**

Run: `npm test -- src/game/heightMeasurement.test.ts`

- [ ] **Step 3: Implement the pure reducer**

```ts
export interface HeightBound { maxY: number }

export function getTowerHeight(bounds: readonly HeightBound[]): number {
  const highest = bounds.reduce((value, bound) => Math.max(value, bound.maxY), 0)
  return Math.round(Math.max(0, highest) * 100) / 100
}
```

- [ ] **Step 4: Run the focused test and confirm it passes**

Run: `npm test -- src/game/heightMeasurement.test.ts`

### Task 2: Safe best-record persistence

**Files:**
- Create: `src/game/bestHeightStorage.ts`
- Create: `src/game/bestHeightStorage.test.ts`

- [ ] **Step 1: Write failing tests with an in-memory storage double**

```ts
import { describe, expect, it } from 'vitest'
import { loadBestHeight, saveBestHeight } from './bestHeightStorage'

describe('best height storage', () => {
  it('round-trips a finite non-negative record', () => {
    localStorage.clear()
    saveBestHeight(4.2, localStorage)
    expect(loadBestHeight(localStorage)).toBe(4.2)
  })

  it.each(['broken', '-2', 'null', '"4"'])('rejects invalid value %s', (value) => {
    localStorage.setItem('tsumitate-game:best-height:v1', value)
    expect(loadBestHeight(localStorage)).toBe(0)
  })
})
```

- [ ] **Step 2: Run the test and confirm it fails because the module is missing**

Run: `npm test -- src/game/bestHeightStorage.test.ts`

- [ ] **Step 3: Implement guarded read and write**

```ts
export const BEST_HEIGHT_STORAGE_KEY = 'tsumitate-game:best-height:v1'
type StorageLike = Pick<Storage, 'getItem' | 'setItem'>

export function loadBestHeight(storage: StorageLike | undefined): number {
  try {
    const value: unknown = JSON.parse(storage?.getItem(BEST_HEIGHT_STORAGE_KEY) ?? '0')
    return typeof value === 'number' && Number.isFinite(value) && value >= 0 ? value : 0
  } catch { return 0 }
}

export function saveBestHeight(value: number, storage: StorageLike | undefined): void {
  if (!Number.isFinite(value) || value < 0) return
  try { storage?.setItem(BEST_HEIGHT_STORAGE_KEY, JSON.stringify(value)) } catch { /* session fallback */ }
}
```

- [ ] **Step 4: Add throwing-storage assertions and run the focused test**

```ts
it('falls back when storage throws', () => {
  const storage = {
    getItem: () => { throw new Error('blocked') },
    setItem: () => { throw new Error('blocked') },
  }
  expect(loadBestHeight(storage)).toBe(0)
  expect(() => saveBestHeight(3.2, storage)).not.toThrow()
})
```

Run: `npm test -- src/game/bestHeightStorage.test.ts`

### Task 3: Measure Rapier bodies and gate records

**Files:**
- Modify: `src/components/GameScene.tsx`
- Modify: `src/App.tsx`

- [ ] **Step 1: Add `onHeightChange: (height: number) => void` to `GameSceneProps`**

```ts
interface GameSceneProps {
  // existing props
  onHeightChange: (height: number) => void
}
```

- [ ] **Step 2: Use `useFrame` in `SceneContent` to sample at 10 Hz**

```ts
const elapsed = useRef(0)
const lastHeight = useRef(-1)
useFrame((_, delta) => {
  if (!props.containmentEnabled) return
  elapsed.current += delta
  if (elapsed.current < 0.1) return
  elapsed.current = 0
  const bounds = [...bodies.current.values()]
    .filter((body) => body.isValid())
    .flatMap((body) => Array.from({ length: body.numColliders() }, (_, index) => ({
      maxY: body.collider(index).computeAABB().max.y,
    })))
  const height = getTowerHeight(bounds)
  if (height !== lastHeight.current) {
    lastHeight.current = height
    props.onHeightChange(height)
  }
})
```

- [ ] **Step 3: Add `currentHeight` and initialized `bestHeight` state to `App`**

```ts
const [currentHeight, setCurrentHeight] = useState(0)
const [bestHeight, setBestHeight] = useState(() =>
  loadBestHeight(typeof window === 'undefined' ? undefined : window.localStorage),
)
```

- [ ] **Step 4: Accept measurements only while containment is active, save new records, freeze on destroy, and clear only current height on reset**

```ts
const handleHeightChange = useCallback((height: number) => {
  if (!state.containmentEnabled) return
  setCurrentHeight(height)
  setBestHeight((best) => {
    if (height <= best) return best
    saveBestHeight(height, window.localStorage)
    return height
  })
}, [state.containmentEnabled])

const handleReset = () => {
  setCurrentHeight(0)
  dispatch({ type: 'reset' })
}
```

- [ ] **Step 5: Run typecheck to validate Rapier collider access**

Run: `npm run typecheck`

### Task 4: Render laboratory UI and scene decoration

**Files:**
- Create: `src/components/MeasurementGuide.tsx`
- Modify: `src/components/GameScene.tsx`
- Modify: `src/components/GameHud.tsx`
- Modify: `src/components/GameHud.test.tsx`
- Modify: `src/styles.css`

- [ ] **Step 1: Extend the HUD test with current and best values**

```tsx
expect(screen.getByText('4.20')).toBeInTheDocument()
expect(screen.getByText('5.80 m')).toBeInTheDocument()
```

- [ ] **Step 2: Run the HUD test and confirm the new metrics are missing**

Run: `npm test -- src/components/GameHud.test.tsx`

- [ ] **Step 3: Add `currentHeight` and `bestHeight` props and a `measurement-panel` to `GameHud`**

```tsx
<aside className="measurement-panel" aria-label="高さ計測">
  <span>TOWER HEIGHT</span>
  <strong>{props.currentHeight.toFixed(2)} <small>m</small></strong>
  <div><span>PERSONAL BEST</span><b>{props.bestHeight.toFixed(2)} m</b></div>
</aside>
```

- [ ] **Step 4: Implement `MeasurementGuide` with Drei `Grid`, `Sparkles`, `Line`, and `Text`, using `currentHeight` for the live marker**

```tsx
export function MeasurementGuide({ height }: { height: number }) {
  const ticks = [0, 2, 4, 6, 8]
  return <>
    <Grid args={[30, 30]} position={[0, -0.01, 0]} cellColor="#164052" sectionColor="#42cfe8" fadeDistance={24} infiniteGrid />
    <Sparkles count={32} scale={[15, 8, 15]} color="#69e6ff" size={1.4} speed={0.18} opacity={0.35} />
    <Line points={[[6.2, 0, 0], [6.2, 8, 0]]} color="#69e6ff" transparent opacity={0.7} />
    {ticks.map((tick) => <group key={tick} position={[6.2, tick, 0]}>
      <Line points={[[0, 0, 0], [-0.25, 0, 0]]} color="#69e6ff" />
      <Text position={[0.4, 0, 0]} fontSize={0.22} color="#9addeb">{tick}m</Text>
    </group>)}
    {height > 0 && <Line points={[[-2.5, height, 0], [6.2, height, 0]]} color="#b9ff69" dashed dashSize={0.18} gapSize={0.12} />}
  </>
}
```

- [ ] **Step 5: Place `MeasurementGuide` in `SceneContent` and shift scene colors toward the approved cyan laboratory palette**

```tsx
<color attach="background" args={['#0b111a']} />
<fog attach="fog" args={['#0b111a', 18, 42]} />
<MeasurementGuide height={props.currentHeight} />
```

- [ ] **Step 6: Add desktop and mobile CSS for the measurement panel without changing control positions**

```css
.measurement-panel { position:absolute; top:clamp(18px,3vw,42px); left:clamp(18px,3vw,42px); }
@media (max-width:720px) { .measurement-panel { top:16px; left:12px; transform:scale(.82); transform-origin:top left; } }
```

- [ ] **Step 7: Run the HUD test and typecheck**

Run: `npm test -- src/components/GameHud.test.tsx && npm run typecheck`

### Task 5: Full verification

**Files:**
- Verify all files above

- [ ] **Step 1: Run the complete automated checks**

Run: `npm test && npm run typecheck && npm run build && git diff --check`

- [ ] **Step 2: Verify in the live browser**

Confirm desktop and mobile layouts, live height changes after placement, current height reset, best-height persistence after reload, no control overlap, and no browser console errors.

- [ ] **Step 3: Commit the implementation on main**

```bash
git add games/tsumitate-game/src
git commit -m "feat: add laboratory height measurements"
```
