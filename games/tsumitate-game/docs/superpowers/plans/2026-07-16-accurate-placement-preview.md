# Accurate Placement Preview Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the full vertical space above the platform clickable, project clicks accurately to the platform, clamp outside projections inside the circle, and show the current daily-deck piece as a ghost preview.

**Architecture:** Pure vector helpers compute the ray/ground intersection and radial clamp. `ElevatedDropZone` renders one camera-enclosing invisible sphere, uses the same helper for pointer preview and click placement, and renders a non-interactive `PlacementGhost`.

**Tech Stack:** React 19, TypeScript, React Three Fiber, Three.js, Vitest

---

### Task 1: Correct ray projection

**Files:** `src/game/placement.ts`, `src/game/placement.test.ts`

- [ ] Write failing tests for angled ray projection, radial clamp, existing surface priority, drag rejection, and levels `[3,6,9,12,15,18,21,24]`.
- [ ] Replace elevated-plane coordinates with ray origin/direction intersection at `y=0`.
- [ ] Clamp an outside point to the configured inner radius.
- [ ] Run the focused test.

### Task 2: Full-scene click zone and ghost

**Files:** `src/components/ElevatedDropZone.tsx`, `src/components/PlacementGhost.tsx`, `src/components/GameScene.tsx`, `src/App.tsx`

- [ ] Render one invisible back-sided sphere large enough to contain every allowed camera position.
- [ ] Store the latest projected point during pointer movement and clear it when leaving.
- [ ] Render current piece geometry, scale, color-independent cyan wireframe, and initial rotation at the preview point.
- [ ] Disable ghost raycasting, shadows, depth writing, and physics.
- [ ] Pass the current daily-deck piece from `App` through `GameScene`.
- [ ] Run typecheck and focused tests.

### Task 3: Verification

- [ ] Run `npm test && npm run typecheck && npm run build && git diff --check`.
- [ ] Verify the dev server is available and the updated page can be reloaded.
- [ ] Commit implementation files on main with `fix: align placement preview and drop position`.
