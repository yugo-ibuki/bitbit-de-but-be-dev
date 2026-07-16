# Elevated Drop Zone and Wall Recovery Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Allow placement from an invisible elevated circular target while restoring the containment wall three seconds after the latest destruction.

**Architecture:** A pure placement helper chooses an existing marked surface intersection when present and otherwise projects the elevated hit to platform height. A focused recovery hook owns the restartable timer and dispatches a new reducer action that re-enables containment.

**Tech Stack:** React 19, TypeScript, React Three Fiber, Rapier, Vitest, Testing Library

---

## File structure

- Create `src/components/ElevatedDropZone.tsx`: invisible circular click surface and R3F event adapter.
- Create `src/game/placement.ts`: pure click-to-placement conversion and drop-zone constants.
- Create `src/game/placement.test.ts`: surface priority, projection, and drag rejection tests.
- Create `src/game/useContainmentRecovery.ts`: restartable three-second wall timer.
- Create `src/game/useContainmentRecovery.test.ts`: timing, restart, and cancellation tests.
- Modify `src/components/GameScene.tsx`: mount the elevated zone and mark the platform as a placement surface.
- Modify `src/components/StackingObject.tsx`: mark object meshes as placement surfaces.
- Modify `src/game/gameState.ts`: add the wall recovery action.
- Modify `src/game/gameState.test.ts`: verify destroy, restore, repeated destroy, and reset states.
- Modify `src/App.tsx`: connect the recovery hook to the reducer.

### Task 1: Pure elevated placement rules

**Files:**
- Create: `src/game/placement.ts`
- Create: `src/game/placement.test.ts`

- [ ] **Step 1: Write failing placement tests**

```ts
import { describe, expect, it } from 'vitest'
import { getElevatedPlacementPoint } from './placement'

describe('getElevatedPlacementPoint', () => {
  it('projects an empty elevated hit to platform height', () => {
    expect(getElevatedPlacementPoint([2, 4.5, -1], [], 0)).toEqual([2, 0, -1])
  })

  it('preserves the nearest existing placement surface', () => {
    expect(getElevatedPlacementPoint([2, 4.5, -1], [[1, 2.2, 0]], 0)).toEqual([1, 2.2, 0])
  })

  it('rejects a camera drag', () => {
    expect(getElevatedPlacementPoint([2, 4.5, -1], [], 7)).toBeNull()
  })
})
```

- [ ] **Step 2: Run `npm test -- src/game/placement.test.ts` and confirm the module-missing failure**

- [ ] **Step 3: Implement constants and conversion**

```ts
import type { Vec3 } from './types'

export const ELEVATED_DROP_ZONE_HEIGHT = 4.5
export const ELEVATED_DROP_ZONE_RADIUS = 6.3
export const MAX_PLACEMENT_DRAG = 6

export function getElevatedPlacementPoint(
  elevatedPoint: Vec3,
  placementSurfacePoints: readonly Vec3[],
  dragDistance: number,
): Vec3 | null {
  if (dragDistance > MAX_PLACEMENT_DRAG) return null
  return placementSurfacePoints[0] ?? [elevatedPoint[0], 0, elevatedPoint[2]]
}
```

- [ ] **Step 4: Run the focused test and confirm all three cases pass**

### Task 2: Restartable containment recovery

**Files:**
- Create: `src/game/useContainmentRecovery.ts`
- Create: `src/game/useContainmentRecovery.test.ts`
- Modify: `src/game/gameState.ts`
- Modify: `src/game/gameState.test.ts`

- [ ] **Step 1: Add failing reducer assertions**

```ts
const destroyed = gameReducer(placedState, { type: 'destroy' })
expect(destroyed.containmentEnabled).toBe(false)
expect(gameReducer(destroyed, { type: 'restore-containment' }).containmentEnabled).toBe(true)
expect(gameReducer(destroyed, { type: 'reset' }).containmentEnabled).toBe(true)
```

- [ ] **Step 2: Add the reducer action and implementation**

```ts
export type GameAction =
  | { type: 'restore-containment' }
  // existing actions

case 'restore-containment':
  return { ...state, containmentEnabled: true }
```

- [ ] **Step 3: Write failing hook tests with fake timers**

```ts
import { renderHook } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { useContainmentRecovery } from './useContainmentRecovery'

describe('useContainmentRecovery', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('restores three seconds after destruction and restarts for a later destruction', () => {
    const restore = vi.fn()
    const { rerender } = renderHook(
      ({ version }) => useContainmentRecovery(false, version, restore),
      { initialProps: { version: 1 } },
    )
    vi.advanceTimersByTime(2000)
    rerender({ version: 2 })
    vi.advanceTimersByTime(2999)
    expect(restore).not.toHaveBeenCalled()
    vi.advanceTimersByTime(1)
    expect(restore).toHaveBeenCalledOnce()
  })

  it('cancels recovery when containment is already enabled', () => {
    const restore = vi.fn()
    const { rerender } = renderHook(
      ({ enabled }) => useContainmentRecovery(enabled, 1, restore),
      { initialProps: { enabled: false } },
    )
    rerender({ enabled: true })
    vi.advanceTimersByTime(3000)
    expect(restore).not.toHaveBeenCalled()
  })
})
```

- [ ] **Step 4: Implement the hook**

```ts
import { useEffect } from 'react'

export const CONTAINMENT_RECOVERY_MS = 3000

export function useContainmentRecovery(
  containmentEnabled: boolean,
  destructionVersion: number,
  onRestore: () => void,
) {
  useEffect(() => {
    if (containmentEnabled || destructionVersion === 0) return
    const timer = window.setTimeout(onRestore, CONTAINMENT_RECOVERY_MS)
    return () => window.clearTimeout(timer)
  }, [containmentEnabled, destructionVersion, onRestore])
}
```

- [ ] **Step 5: Run the reducer and hook tests and confirm they pass**

### Task 3: Connect the 3D click target and recovery timer

**Files:**
- Create: `src/components/ElevatedDropZone.tsx`
- Modify: `src/components/GameScene.tsx`
- Modify: `src/components/StackingObject.tsx`
- Modify: `src/App.tsx`

- [ ] **Step 1: Implement the invisible click target**

```tsx
import type { ThreeEvent } from '@react-three/fiber'
import { ELEVATED_DROP_ZONE_HEIGHT, ELEVATED_DROP_ZONE_RADIUS, getElevatedPlacementPoint } from '../game/placement'
import type { Vec3 } from '../game/types'

export function ElevatedDropZone({ onPlace }: { onPlace: (point: Vec3) => void }) {
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

  return <mesh position={[0, ELEVATED_DROP_ZONE_HEIGHT, 0]} rotation={[-Math.PI / 2, 0, 0]} onClick={handleClick}>
    <circleGeometry args={[ELEVATED_DROP_ZONE_RADIUS, 64]} />
    <meshBasicMaterial transparent opacity={0} depthWrite={false} />
  </mesh>
}
```

- [ ] **Step 2: Mark platform and stacking meshes with `userData={{ placementSurface: true }}` and mount `<ElevatedDropZone onPlace={props.onPlace} />` in `SceneContent`**

```tsx
<mesh userData={{ placementSurface: true }} receiveShadow position={[0, -0.6, 0]} onClick={handlePlace}>
<mesh userData={{ placementSurface: true }} castShadow receiveShadow onClick={onPlace}>
<ElevatedDropZone onPlace={props.onPlace} />
```

- [ ] **Step 3: Connect the recovery hook in `App` with a stable callback**

```ts
const restoreContainment = useCallback(
  () => dispatch({ type: 'restore-containment' }),
  [],
)
useContainmentRecovery(
  state.containmentEnabled,
  state.destructionVersion,
  restoreContainment,
)
```

- [ ] **Step 4: Run `npm test && npm run typecheck` and resolve no warnings or failures**

### Task 4: Browser and build verification

**Files:**
- Verify all files above

- [ ] **Step 1: Run `npm test && npm run typecheck && npm run build && git diff --check`**

- [ ] **Step 2: In the live browser, click above the visible platform and confirm one object appears; drag the camera and confirm no object is added**

- [ ] **Step 3: Destroy a stack, wait three seconds, place another object, and confirm it remains constrained by the restored wall**

- [ ] **Step 4: Commit scoped implementation files on main with `feat: expand placement and restore containment`**
