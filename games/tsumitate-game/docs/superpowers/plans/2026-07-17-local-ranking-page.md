# Local Ranking Page Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Save completed destruction attempts and display a polished device-local daily ranking on a dedicated page.

**Architecture:** A pure storage module validates, sorts, and persists ranking entries. A small History API hook switches between the game and `/ranking` without unmounting the game, while a focused ranking component renders summary and top-10 data.

**Tech Stack:** React 19, TypeScript, Vitest, Testing Library, browser localStorage and History API

---

### Task 1: Ranking storage and ordering

**Files:**
- Create: `src/game/rankingStorage.test.ts`
- Create: `src/game/rankingStorage.ts`

- [ ] **Step 1: Write failing storage tests**

Test that `saveRankingEntry()` isolates challenge dates, sorts by height then piece efficiency then time, rejects invalid JSON records, and caps results at 20.

```ts
saveRankingEntry('2026-07-17', { id: 'a', height: 4.2, usedCount: 20, completedAt: 2 }, localStorage)
saveRankingEntry('2026-07-17', { id: 'b', height: 5.1, usedCount: 25, completedAt: 3 }, localStorage)
expect(loadRankingEntries('2026-07-17', localStorage).map(({ id }) => id)).toEqual(['b', 'a'])
```

- [ ] **Step 2: Verify the tests fail**

Run `npm test -- src/game/rankingStorage.test.ts` and expect failure because `rankingStorage.ts` does not exist.

- [ ] **Step 3: Implement the storage boundary**

Define `RankingEntry`, `getRankingStorageKey`, `sortRankingEntries`, `loadRankingEntries`, and `saveRankingEntry`. Parse unknown JSON defensively, round stored heights to two decimals, retain the best 20 entries, and swallow storage access failures.

```ts
export interface RankingEntry {
  id: string
  height: number
  usedCount: number
  completedAt: number
}

export function sortRankingEntries(entries: RankingEntry[]): RankingEntry[] {
  return [...entries].sort((a, b) =>
    b.height - a.height || a.usedCount - b.usedCount || a.completedAt - b.completedAt,
  )
}
```

- [ ] **Step 4: Verify storage tests pass**

Run `npm test -- src/game/rankingStorage.test.ts` and expect all tests to pass.

- [ ] **Step 5: Commit storage behavior**

Commit `src/game/rankingStorage.ts` and `src/game/rankingStorage.test.ts` with message `feat: store daily ranking attempts`.

### Task 2: App route hook

**Files:**
- Create: `src/game/useAppRoute.test.tsx`
- Create: `src/game/useAppRoute.ts`

- [ ] **Step 1: Write failing route tests**

Render a harness for `useAppRoute()`, assert `/ranking` maps to `ranking`, navigation pushes the correct path, and a dispatched `popstate` updates the active route.

```tsx
function Harness() {
  const { route, navigate } = useAppRoute()
  return <button onClick={() => navigate('ranking')}>{route}</button>
}
```

- [ ] **Step 2: Verify the route tests fail**

Run `npm test -- src/game/useAppRoute.test.tsx` and expect failure because the hook does not exist.

- [ ] **Step 3: Implement two-route navigation**

Expose `route: 'game' | 'ranking'` and `navigate(route)`. Derive the initial value from `window.location.pathname`, call `history.pushState`, and subscribe to `popstate` with cleanup.

```ts
export type AppRoute = 'game' | 'ranking'
export function getRoute(pathname: string): AppRoute {
  return pathname === '/ranking' ? 'ranking' : 'game'
}
```

- [ ] **Step 4: Verify route tests pass**

Run `npm test -- src/game/useAppRoute.test.tsx` and expect all tests to pass.

- [ ] **Step 5: Commit routing behavior**

Commit the hook and tests with message `feat: add game ranking navigation`.

### Task 3: Ranking page presentation

**Files:**
- Create: `src/components/RankingPage.test.tsx`
- Create: `src/components/RankingPage.tsx`
- Modify: `src/styles.css`

- [ ] **Step 1: Write failing component tests**

Assert that the empty state is visible without entries and that populated data renders ranks, formatted heights, piece counts, summary values, and calls `onBack`.

```tsx
render(<RankingPage challengeKey="2026-07-17" entries={[entry]} onBack={onBack} />)
expect(screen.getByText('5.42 m')).toBeInTheDocument()
expect(screen.getByText('18 個')).toBeInTheDocument()
```

- [ ] **Step 2: Verify component tests fail**

Run `npm test -- src/components/RankingPage.test.tsx` and expect failure because the component does not exist.

- [ ] **Step 3: Build the ranking view**

Render a header, local-data badge, two summary cards, top-10 table, medal styling for ranks one through three, empty-state call to action, and back button. Use semantic headings, table markup, and button labels.

- [ ] **Step 4: Add responsive ranking styles**

Add `.ranking-page`, `.ranking-shell`, `.ranking-summary`, `.ranking-table`, `.rank-position`, and mobile rules. Preserve the existing cyan/lime lab palette, allow vertical scrolling, and hide the completion time column below 560px.

- [ ] **Step 5: Verify component tests pass**

Run `npm test -- src/components/RankingPage.test.tsx` and expect all tests to pass.

- [ ] **Step 6: Commit the ranking page**

Commit component, tests, and styles with message `feat: add local ranking page`.

### Task 4: Record attempts and wire navigation

**Files:**
- Modify: `src/components/GameHud.test.tsx`
- Modify: `src/components/GameHud.tsx`
- Modify: `src/App.tsx`

- [ ] **Step 1: Write a failing HUD navigation test**

Pass `onOpenRanking` to `GameHud`, click the `ランキング` button, and assert the callback runs once.

- [ ] **Step 2: Verify the HUD test fails**

Run `npm test -- src/components/GameHud.test.tsx` and expect failure because the navigation prop and button are absent.

- [ ] **Step 3: Add the ranking HUD control**

Add `onOpenRanking: () => void` to `GameHudProps` and render a compact `ランキング` button adjacent to the measurement card with matching lab styling.

- [ ] **Step 4: Wire attempts and routes in App**

Use `useAppRoute`, load daily entries once, and save an entry before the first valid destruction in an attempt. Track whether the current attempt has already been recorded in a ref; clear it only when the player resets. Render the ranking page above the still-mounted game shell when the ranking route is active.

```ts
const handleDestroy = useCallback(() => {
  if (state.items.length > 0 && currentHeight > 0 && !attemptRecorded.current) {
    const entry = { id: crypto.randomUUID(), height: currentHeight, usedCount: state.usedCount, completedAt: Date.now() }
    setRankingEntries(saveRankingEntry(challengeKey, entry, storage))
    attemptRecorded.current = true
  }
  dispatch({ type: 'destroy' })
}, [challengeKey, currentHeight, state])
```

- [ ] **Step 5: Verify focused and full tests pass**

Run `npm test -- src/components/GameHud.test.tsx` followed by `npm test`; expect all tests to pass.

- [ ] **Step 6: Commit application integration**

Commit `App.tsx`, `GameHud.tsx`, `GameHud.test.tsx`, and related styles with message `feat: connect attempts to daily ranking`.

### Task 5: Final verification

**Files:**
- Modify only files needed to fix verification findings.

- [ ] **Step 1: Run static verification**

Run `npm run typecheck`, `npm run build`, and `git diff --check`; expect zero errors.

- [ ] **Step 2: Verify the game in the browser**

Open `http://localhost:5173/`, stack at least one object, press destroy, open the ranking page, confirm the saved height and used count, return to the game, and confirm the scene still works.

- [ ] **Step 3: Verify responsive behavior**

At a narrow viewport, confirm the page scrolls, controls do not overlap, and the time column is hidden.

- [ ] **Step 4: Commit any verification fixes**

If verification required changes, commit only those changes with message `fix: polish ranking experience`.
