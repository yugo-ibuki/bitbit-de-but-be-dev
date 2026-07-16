# Local Ranking Page Design

## Goal

Add a ranking page that gives repeated stacking attempts a visible result without requiring an external account or backend. The first version ranks attempts saved on the current device for the current daily challenge.

## Scope

- Save one result when the player presses the destroy button and the tower has a measurable height.
- Store the challenge date, height, used piece count, and completion time.
- Keep at most 20 results per challenge day in browser storage.
- Rank results by height descending, then by fewer used pieces, then by earlier completion time.
- Add a dedicated `/ranking` page with the day's top 10 results, best height, and attempt count.
- Add navigation between the game and ranking without losing an in-progress game during the current browser session.
- Clearly label the leaderboard as device-local.

## Architecture

`rankingStorage.ts` owns the result schema, validation, ordering, and local-storage persistence. React components do not parse stored JSON directly. This creates a small storage boundary that can later be replaced by a remote API while preserving the page component.

`useAppRoute.ts` owns the two supported routes, `/` and `/ranking`, using the History API and `popstate`. No router dependency is needed for two pages.

`RankingPage.tsx` is a presentation component. It receives already-ranked entries and summary values, and exposes a back action.

`App.tsx` keeps the game state mounted while switching views so returning from the ranking page preserves the current stack. It records the current height before dispatching destruction, refreshes ranking data after saving, and provides page navigation.

## Interaction Design

The existing lab aesthetic continues on the ranking page: dark blue background, cyan measurement accents, lime highlights, compact uppercase labels, and a wide central results panel.

The HUD gets a compact `RANKING` button near the measurement panel. The ranking page contains:

- a back-to-game button;
- the current challenge date and `この端末の記録` label;
- summary cards for best height and number of recorded attempts;
- a top-10 table with rank, height, used pieces, and time;
- an empty state that directs the player to stack and press destroy.

The first three ranks receive distinct visual emphasis. Mobile uses a full-height scrollable page and hides the least important time column when space is tight.

## Data Rules

A destruction is recorded only when the stack contains at least one item and the measured height is greater than zero. Repeated presses after containment has been removed do not create duplicate results for the same destruction cycle. Resetting starts a new attempt.

Invalid or outdated browser data is ignored safely. A failed storage write does not interrupt gameplay.

## Testing

- Unit-test result validation, ordering, daily isolation, and the 20-entry limit.
- Component-test the ranking page's populated and empty states.
- Component-test the HUD ranking navigation action.
- Run the complete Vitest suite, TypeScript typecheck, production build, and a browser check of both routes at desktop and narrow widths.

## Deferred

- Shared online rankings, accounts, player names, anti-cheat validation, and server persistence.
- Historical date browsing.
- Replays or tower screenshots.
