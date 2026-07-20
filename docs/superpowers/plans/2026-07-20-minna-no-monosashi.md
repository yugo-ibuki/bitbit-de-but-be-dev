# みんなのものさし MVP Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 管理者が最大10問の質問ブロックを公開でき、匿名参加者が各問へ端末単位で1回回答し、回答直後と全問完了後に全体統計を確認できるWebアプリを世界公開する。

**Architecture:** `games/minna-no-monosashi/` にReact/Vite SPAとHono Worker APIを同居させ、Cloudflare Workers Static Assetsで単一デプロイする。D1の一意制約を重複回答防止の最終防衛線とし、React非依存のdomain、HTTPを扱うWorker、D1 repositories、参加者UI、管理UIを分離する。

**Tech Stack:** TypeScript 7、React 19、Vite 8、React Router 7、Hono 4、Cloudflare Workers/D1/Rate Limiting、Vitest 4、React Testing Library、Cloudflare Workers Vitest integration、Playwright 1.61、Wrangler 4

---

## 実装前提

- 作業場所はユーザー指定により専用worktreeではなく、このリポジトリの `main` とする。
- 各タスク開始時に `git status --short` を確認し、他の作業による変更を取り込まない。
- 各タスクは失敗するテスト、最小実装、成功確認、対象ファイルだけのコミット、の順で進める。
- パッケージ操作と全コマンドは `games/minna-no-monosashi/` 内で実行する。
- 設計の根拠は `docs/superpowers/specs/2026-07-20-minna-no-monosashi-design.md` とする。

## ファイル構成

```text
games/minna-no-monosashi/
├── .dev.vars.example                 # ローカル秘密値の形式と安全な開発値
├── .gitignore                        # node_modules、dist、.dev.vars、Playwright成果物
├── README.md                         # 開発、DB、seed、秘密値、デプロイ手順
├── index.html                        # SPAエントリ
├── package.json                      # 独立した依存関係と実行コマンド
├── playwright.config.ts              # モバイル・デスクトップE2E
├── tsconfig.json                     # client/worker共通型チェック
├── vite.config.ts                    # React + Cloudflare Vite plugin
├── vitest.config.ts                  # domain/UIのjsdomテスト
├── vitest.worker.config.ts           # workerd + D1 API統合テスト
├── wrangler.jsonc                    # Worker、assets、D1、rate limit bindings
├── migrations/
│   └── 0001_initial.sql              # blocks/questions/options/responses
├── seed/
│   └── sample-block.sql              # 再実行可能な独自10問ブロック
├── scripts/
│   └── hash-admin-password.mjs       # PBKDF2ハッシュ生成
├── e2e/
│   └── app.spec.ts                   # 管理・回答・終了の主要フロー
├── test/
│   ├── apply-migrations.ts           # Workerテスト用D1初期化
│   └── worker/
│       ├── helpers.ts
│       ├── admin-api.test.ts
│       ├── public-api.test.ts
│       ├── security.test.ts
│       └── vote-api.test.ts
└── src/
    ├── main.tsx                      # React起動
    ├── test/setup.ts                 # jest-dom
    ├── app/
    │   ├── App.tsx                   # BrowserRouterと全ルート
    │   └── App.test.tsx              # ルーティングsmoke test
    ├── shared/
    │   └── contracts.ts              # API request/response/error型
    ├── domain/
    │   ├── blockRules.ts
    │   ├── blockRules.test.ts
    │   ├── percentages.ts
    │   ├── percentages.test.ts
    │   └── types.ts
    ├── client/
    │   ├── api/client.ts             # fetchとApiError
    │   ├── deviceToken.ts             # localStorage/session fallback
    │   ├── deviceToken.test.ts
    │   ├── components/
    │   │   ├── AppLayout.tsx
    │   │   ├── AsyncState.tsx
    │   │   ├── QuestionForm.tsx
    │   │   ├── ResultChart.tsx
    │   │   └── ResultChart.test.tsx
    │   ├── pages/
    │   │   ├── HomePage.tsx
    │   │   ├── BlockIntroPage.tsx
    │   │   ├── QuestionPage.tsx
    │   │   ├── QuestionPage.test.tsx
    │   │   ├── QuestionResultPage.tsx
    │   │   ├── SummaryPage.tsx
    │   │   └── ClosedResultsPage.tsx
    │   ├── admin/
    │   │   ├── AdminLoginPage.tsx
    │   │   ├── AdminDashboardPage.tsx
    │   │   ├── BlockEditorPage.tsx
    │   │   ├── BlockEditorPage.test.tsx
    │   │   └── AdminResultsPage.tsx
    │   └── styles.css
    └── worker/
        ├── index.ts                   # ExportedHandler
        ├── app.ts                     # Hono composition
        ├── env.ts                     # bindings型
        ├── auth/
        │   ├── crypto.ts              # PBKDF2、HMAC、timing-safe比較
        │   ├── session.ts             # 署名Cookie
        │   └── csrf.ts                # Origin/CSRF検証
        ├── http/
        │   ├── errors.ts              # AppErrorとJSON変換
        │   └── securityHeaders.ts      # CSP等
        ├── repositories/
        │   ├── blockRepository.ts
        │   └── responseRepository.ts
        ├── services/
        │   ├── blockService.ts
        │   └── responseService.ts
        └── routes/
            ├── publicRoutes.ts
            ├── responseRoutes.ts
            └── adminRoutes.ts
```

### Task 1: Cloudflare Reactアプリの土台

**Files:**
- Create: `games/minna-no-monosashi/package.json`
- Create: `games/minna-no-monosashi/tsconfig.json`
- Create: `games/minna-no-monosashi/vite.config.ts`
- Create: `games/minna-no-monosashi/vitest.config.ts`
- Create: `games/minna-no-monosashi/wrangler.jsonc`
- Create: `games/minna-no-monosashi/index.html`
- Create: `games/minna-no-monosashi/src/test/setup.ts`
- Create: `games/minna-no-monosashi/src/client/styles.css`
- Test: `games/minna-no-monosashi/src/app/App.test.tsx`
- Create: `games/minna-no-monosashi/src/app/App.tsx`
- Create: `games/minna-no-monosashi/src/main.tsx`

- [ ] **Step 1: パッケージとCloudflare設定を作る**

`package.json` は次のscriptと固定バージョンを持たせる。

```json
{
  "name": "minna-no-monosashi",
  "private": true,
  "version": "0.1.0",
  "type": "module",
  "engines": { "node": ">=22" },
  "scripts": {
    "dev": "vite",
    "build": "tsc -b && vite build",
    "typecheck": "tsc -b",
    "test": "npm run test:unit && npm run test:worker",
    "test:unit": "vitest run --config vitest.config.ts",
    "test:worker": "vitest run --config vitest.worker.config.ts",
    "test:e2e": "playwright test",
    "preview": "vite preview",
    "cf-typegen": "wrangler types",
    "db:migrate:local": "wrangler d1 migrations apply DB --local",
    "db:migrate:remote": "wrangler d1 migrations apply DB --remote",
    "db:seed:local": "wrangler d1 execute DB --local --file=seed/sample-block.sql",
    "db:seed:remote": "wrangler d1 execute DB --remote --file=seed/sample-block.sql",
    "deploy": "npm run build && wrangler deploy"
  },
  "dependencies": {
    "hono": "4.12.31",
    "react": "19.2.7",
    "react-dom": "19.2.7",
    "react-router-dom": "7.18.1"
  },
  "devDependencies": {
    "@cloudflare/vite-plugin": "1.45.1",
    "@cloudflare/vitest-pool-workers": "0.18.6",
    "@cloudflare/workers-types": "5.20260719.1",
    "@playwright/test": "1.61.1",
    "@testing-library/jest-dom": "6.9.1",
    "@testing-library/react": "16.3.2",
    "@testing-library/user-event": "14.6.1",
    "@types/node": "26.1.1",
    "@types/react": "19.2.17",
    "@types/react-dom": "19.2.3",
    "@vitejs/plugin-react": "6.0.3",
    "jsdom": "29.1.1",
    "typescript": "7.0.2",
    "vite": "8.1.5",
    "vitest": "4.1.10",
    "wrangler": "4.112.0"
  }
}
```

`wrangler.jsonc` はSPA fallbackより先に `/api/*` をWorkerへ通す。

```jsonc
{
  "$schema": "./node_modules/wrangler/config-schema.json",
  "name": "minna-no-monosashi",
  "main": "src/worker/index.ts",
  "compatibility_date": "2026-07-20",
  "assets": {
    "not_found_handling": "single-page-application",
    "run_worker_first": ["/api/*"]
  },
  "d1_databases": [{
    "binding": "DB",
    "database_name": "minna-no-monosashi",
    "database_id": "00000000-0000-0000-0000-000000000000",
    "migrations_dir": "migrations"
  }],
  "ratelimits": [
    { "name": "VOTE_RATE_LIMITER", "namespace_id": "1001", "simple": { "limit": 20, "period": 60 } },
    { "name": "LOGIN_RATE_LIMITER", "namespace_id": "1002", "simple": { "limit": 5, "period": 60 } }
  ]
}
```

残りの設定は次の内容にする。

```ts
// vite.config.ts
import { cloudflare } from '@cloudflare/vite-plugin'
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'

export default defineConfig({ plugins: [react(), cloudflare()] })
```

```ts
// vitest.config.ts
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [react()],
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['./src/test/setup.ts'],
    include: ['src/**/*.test.{ts,tsx}'],
  },
})
```

```json
{
  "compilerOptions": {
    "target": "ES2023",
    "useDefineForClassFields": true,
    "lib": ["ES2023", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "allowImportingTsExtensions": false,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "jsx": "react-jsx",
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "types": ["vite/client", "vitest/globals", "@cloudflare/workers-types"]
  },
  "include": ["src", "test", "e2e", "*.ts"]
}
```

`index.html` は `<div id="root"></div>` と `<script type="module" src="/src/main.tsx"></script>` を持たせる。`src/test/setup.ts` は `import '@testing-library/jest-dom/vitest'` の1行、初期 `src/client/styles.css` はbody marginを0にするだけとする。

- [ ] **Step 2: 依存関係をインストールする**

Run: `npm install`

Expected: `package-lock.json` が生成され、exit 0。

- [ ] **Step 3: 最初の失敗するUIテストを書く**

```tsx
import { render, screen } from '@testing-library/react'
import { App } from './App'

it('shows the product name', () => {
  render(<App />)
  expect(screen.getByRole('heading', { name: 'みんなのものさし' })).toBeInTheDocument()
})
```

- [ ] **Step 4: テストが失敗することを確認する**

Run: `npm run test:unit -- src/app/App.test.tsx`

Expected: FAIL because `./App` does not exist。

- [ ] **Step 5: 最小のReactエントリを実装する**

```tsx
// src/app/App.tsx
export function App() {
  return <main><h1>みんなのものさし</h1></main>
}
```

```tsx
// src/main.tsx
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { App } from './app/App'
import './client/styles.css'

createRoot(document.getElementById('root')!).render(<StrictMode><App /></StrictMode>)
```

- [ ] **Step 6: 単体テスト・型・ビルドを確認する**

Run: `npm run test:unit && npm run typecheck && npm run build`

Expected: 1 test PASS、TypeScript error 0、Vite build exit 0。

- [ ] **Step 7: 土台をコミットする**

```bash
git add games/minna-no-monosashi
git commit -m "feat: scaffold minna no monosashi app"
```

### Task 2: ドメインルールと割合計算

**Files:**
- Create: `games/minna-no-monosashi/src/domain/types.ts`
- Test: `games/minna-no-monosashi/src/domain/blockRules.test.ts`
- Create: `games/minna-no-monosashi/src/domain/blockRules.ts`
- Test: `games/minna-no-monosashi/src/domain/percentages.test.ts`
- Create: `games/minna-no-monosashi/src/domain/percentages.ts`

- [ ] **Step 1: 公開可能条件の失敗テストを書く**

```ts
import { validateDraft, canTransition } from './blockRules'

const valid = {
  title: '日常の境界線', slug: 'daily-boundaries', description: '',
  questions: [{ prompt: '朝早いのは？', options: ['6時', '7時'] }],
}

it('accepts 1 to 10 questions and at least 2 unique options', () => {
  expect(validateDraft(valid)).toEqual([])
  expect(validateDraft({ ...valid, questions: [] })).toContainEqual(expect.objectContaining({ field: 'questions' }))
  expect(validateDraft({ ...valid, questions: Array.from({ length: 11 }, () => valid.questions[0]) })).toContainEqual(expect.objectContaining({ field: 'questions' }))
  expect(validateDraft({ ...valid, questions: [{ prompt: '朝早いのは？', options: ['6時', ' 6時 '] }] })).toContainEqual(expect.objectContaining({ field: 'questions.0.options' }))
})

it('only allows draft to published and published to closed', () => {
  expect(canTransition('draft', 'published')).toBe(true)
  expect(canTransition('published', 'closed')).toBe(true)
  expect(canTransition('closed', 'published')).toBe(false)
})
```

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:unit -- src/domain/blockRules.test.ts`

Expected: FAIL because domain modules do not exist。

- [ ] **Step 3: 型と検証を実装する**

```ts
export type BlockStatus = 'draft' | 'published' | 'closed'
export type DraftQuestionInput = { prompt: string; options: string[] }
export type DraftBlockInput = { title: string; slug: string; description: string; questions: DraftQuestionInput[] }
export type ValidationIssue = { field: string; message: string }

export const normalizeText = (value: string) => value.trim().replace(/\s+/g, ' ')
export const canTransition = (from: BlockStatus, to: BlockStatus) =>
  (from === 'draft' && to === 'published') || (from === 'published' && to === 'closed')

export function validateDraft(input: DraftBlockInput): ValidationIssue[] {
  const issues: ValidationIssue[] = []
  if (!normalizeText(input.title) || input.title.length > 80) issues.push({ field: 'title', message: 'タイトルは1〜80文字で入力してください' })
  if (!/^[a-z0-9-]{3,64}$/.test(input.slug)) issues.push({ field: 'slug', message: 'スラッグは小文字英数字とハイフンの3〜64文字です' })
  if (input.description.length > 400) issues.push({ field: 'description', message: '説明は400文字以内です' })
  if (input.questions.length < 1 || input.questions.length > 10) issues.push({ field: 'questions', message: '質問数は1〜10問です' })
  input.questions.forEach((question, index) => {
    if (!normalizeText(question.prompt) || question.prompt.length > 200) issues.push({ field: `questions.${index}.prompt`, message: '質問は1〜200文字です' })
    const normalized = question.options.map(normalizeText)
    if (normalized.length < 2 || normalized.some((option) => !option || option.length > 80) || new Set(normalized).size !== normalized.length) {
      issues.push({ field: `questions.${index}.options`, message: '選択肢は重複しない1〜80文字を2個以上入力してください' })
    }
  })
  return issues
}
```

- [ ] **Step 4: 割合配分の失敗テストを書く**

```ts
import { allocatePercentages } from './percentages'

it('uses largest remainders so displayed values total 100', () => {
  expect(allocatePercentages([1, 1, 1])).toEqual([34, 33, 33])
  expect(allocatePercentages([0, 0])).toEqual([0, 0])
  expect(allocatePercentages([18, 42, 27, 13])).toEqual([18, 42, 27, 13])
})
```

- [ ] **Step 5: 最大剰余法を実装して全domainテストを通す**

```ts
export function allocatePercentages(counts: number[]): number[] {
  const total = counts.reduce((sum, count) => sum + count, 0)
  if (total === 0) return counts.map(() => 0)
  const exact = counts.map((count) => count * 100 / total)
  const result = exact.map(Math.floor)
  let remainder = 100 - result.reduce((sum, value) => sum + value, 0)
  exact.map((value, index) => ({ index, fraction: value - Math.floor(value) }))
    .sort((a, b) => b.fraction - a.fraction || a.index - b.index)
    .forEach(({ index }) => { if (remainder > 0) { result[index] += 1; remainder -= 1 } })
  return result
}
```

Run: `npm run test:unit -- src/domain`

Expected: all domain tests PASS。

- [ ] **Step 6: コミットする**

```bash
git add games/minna-no-monosashi/src/domain
git commit -m "feat: add question block domain rules"
```

### Task 3: D1スキーマとテスト基盤

**Files:**
- Create: `games/minna-no-monosashi/migrations/0001_initial.sql`
- Create: `games/minna-no-monosashi/vitest.worker.config.ts`
- Create: `games/minna-no-monosashi/test/apply-migrations.ts`
- Test: `games/minna-no-monosashi/test/worker/public-api.test.ts`
- Create: `games/minna-no-monosashi/src/worker/env.ts`
- Create: `games/minna-no-monosashi/src/worker/test-env.d.ts`
- Create: `games/minna-no-monosashi/src/worker/index.ts`
- Create: `games/minna-no-monosashi/src/worker/app.ts`

- [ ] **Step 1: D1マイグレーションを書く**

```sql
PRAGMA foreign_keys = ON;
CREATE TABLE blocks (
  id TEXT PRIMARY KEY, slug TEXT NOT NULL UNIQUE, title TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '', status TEXT NOT NULL CHECK(status IN ('draft','published','closed')),
  created_at TEXT NOT NULL, updated_at TEXT NOT NULL, published_at TEXT, closed_at TEXT
);
CREATE TABLE questions (
  id TEXT PRIMARY KEY, block_id TEXT NOT NULL REFERENCES blocks(id) ON DELETE CASCADE,
  prompt TEXT NOT NULL, position INTEGER NOT NULL, created_at TEXT NOT NULL,
  UNIQUE(block_id, position)
);
CREATE TABLE options (
  id TEXT PRIMARY KEY, question_id TEXT NOT NULL REFERENCES questions(id) ON DELETE CASCADE,
  label TEXT NOT NULL, position INTEGER NOT NULL, created_at TEXT NOT NULL,
  UNIQUE(question_id, position)
);
CREATE TABLE responses (
  id TEXT PRIMARY KEY, block_id TEXT NOT NULL REFERENCES blocks(id),
  question_id TEXT NOT NULL REFERENCES questions(id), option_id TEXT NOT NULL REFERENCES options(id),
  voter_key_hash TEXT NOT NULL, created_at TEXT NOT NULL,
  UNIQUE(question_id, voter_key_hash)
);
CREATE INDEX idx_responses_question_option ON responses(question_id, option_id);
CREATE INDEX idx_responses_block_voter ON responses(block_id, voter_key_hash);
```

- [ ] **Step 2: Worker VitestへD1 migrationsを接続する**

```ts
// vitest.worker.config.ts
import path from 'node:path'
import { cloudflareTest } from '@cloudflare/vitest-pool-workers'
import { readD1Migrations } from '@cloudflare/vitest-pool-workers/config'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [cloudflareTest(async () => ({
    wrangler: { configPath: './wrangler.jsonc' },
    miniflare: { bindings: { TEST_MIGRATIONS: await readD1Migrations(path.join(import.meta.dirname, 'migrations')) } },
  }))],
  test: { setupFiles: ['./test/apply-migrations.ts'], include: ['test/worker/**/*.test.ts'] },
})
```

```ts
// test/apply-migrations.ts
import { applyD1Migrations, env } from 'cloudflare:test'
import { beforeAll } from 'vitest'

beforeAll(async () => {
  await applyD1Migrations(env.DB, env.TEST_MIGRATIONS)
})
```

- [ ] **Step 3: 未実装APIの失敗テストを書く**

```ts
import { SELF } from 'cloudflare:test'
import { expect, it } from 'vitest'

it('returns an empty public block list', async () => {
  const response = await SELF.fetch('https://example.test/api/blocks')
  expect(response.status).toBe(200)
  expect(await response.json()).toEqual({ blocks: [] })
})
```

- [ ] **Step 4: 失敗を確認する**

Run: `npm run test:worker -- test/worker/public-api.test.ts`

Expected: FAIL because Worker entry and route are missing。

- [ ] **Step 5: Env、Hono、security headers、空一覧を実装する**

```ts
export interface Env {
  DB: D1Database
  VOTE_RATE_LIMITER: RateLimit
  LOGIN_RATE_LIMITER: RateLimit
  ADMIN_PASSWORD_HASH: string
  SESSION_SECRET: string
  VOTER_HASH_SECRET: string
  TEST_MIGRATIONS?: D1Migration[]
}
```

```ts
// src/worker/test-env.d.ts
import type { Env } from './env'

declare module 'cloudflare:test' {
  interface ProvidedEnv extends Env {
    TEST_MIGRATIONS: D1Migration[]
  }
}
```

```ts
import { Hono } from 'hono'
import type { Env } from './env'

export function createApp() {
  const app = new Hono<{ Bindings: Env }>()
  app.use('*', async (c, next) => {
    await next()
    c.header('X-Content-Type-Options', 'nosniff')
    c.header('Referrer-Policy', 'strict-origin-when-cross-origin')
    c.header('Content-Security-Policy', "default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self'; connect-src 'self'")
  })
  app.get('/api/blocks', async (c) => c.json({ blocks: [] }))
  return app
}
```

`src/worker/index.ts` は `export default createApp()` のみを公開する。

- [ ] **Step 6: Workerテストと型チェックを通す**

Run: `npm run test:worker -- test/worker/public-api.test.ts && npm run typecheck`

Expected: API test PASS、TypeScript error 0。

- [ ] **Step 7: コミットする**

```bash
git add games/minna-no-monosashi/migrations games/minna-no-monosashi/vitest.worker.config.ts games/minna-no-monosashi/test games/minna-no-monosashi/src/worker
git commit -m "feat: add D1 schema and worker test harness"
```

### Task 4: ブロックrepositoryと公開参照API

**Files:**
- Create: `games/minna-no-monosashi/src/shared/contracts.ts`
- Create: `games/minna-no-monosashi/src/worker/repositories/blockRepository.ts`
- Create: `games/minna-no-monosashi/src/worker/services/blockService.ts`
- Create: `games/minna-no-monosashi/src/worker/routes/publicRoutes.ts`
- Modify: `games/minna-no-monosashi/src/worker/app.ts`
- Test: `games/minna-no-monosashi/test/worker/public-api.test.ts`
- Create: `games/minna-no-monosashi/test/worker/helpers.ts`

- [ ] **Step 1: 公開中・終了済み一覧と詳細の失敗テストを追加する**

`helpers.ts` に、指定状態のblockを作る `seedBlock(db, { id, slug, status })` と、公開block・question・optionsを固定IDで作る `seedPublishedQuestion(db)` を実装する。両方ともD1のparameterized statementだけを使い、各テストからimportする。

```ts
it('lists published and closed blocks but hides drafts', async () => {
  await seedBlock(env.DB, { id: 'draft', slug: 'draft', status: 'draft' })
  await seedBlock(env.DB, { id: 'live', slug: 'live', status: 'published' })
  await seedBlock(env.DB, { id: 'closed', slug: 'closed', status: 'closed' })
  const body = await (await SELF.fetch('https://example.test/api/blocks')).json<{ blocks: { slug: string }[] }>()
  expect(body.blocks.map((block) => block.slug)).toEqual(['live', 'closed'])
})
```

追加で `/api/blocks/live` が質問と選択肢を順番どおり返し、`/api/blocks/draft` が404になることを検証する。

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:worker -- test/worker/public-api.test.ts`

Expected: FAIL because repository still returns an empty list。

- [ ] **Step 3: API契約とD1 queryを実装する**

```ts
export type PublicBlockSummary = {
  slug: string; title: string; description: string; status: 'published' | 'closed'
  questionCount: number; participantCount: number
}
export type PublicOption = { id: string; label: string; position: number }
export type PublicQuestion = { id: string; prompt: string; position: number; options: PublicOption[] }
export type PublicBlockDetail = PublicBlockSummary & { questions: PublicQuestion[] }
```

`listPublic()` は `blocks` と質問数、`COUNT(DISTINCT voter_key_hash)` を集約し、`status IN ('published','closed')` のみ返す。`findPublicBySlug()` は質問と選択肢を別queryで取得し、position順に組み立てる。SQLは全て `.prepare(sql).bind(...values)` を使う。

- [ ] **Step 4: public routeをcompositionへ接続する**

```ts
export function publicRoutes() {
  const routes = new Hono<{ Bindings: Env }>()
  routes.get('/blocks', async (c) => c.json({ blocks: await new BlockRepository(c.env.DB).listPublic() }))
  routes.get('/blocks/:slug', async (c) => {
    const block = await new BlockRepository(c.env.DB).findPublicBySlug(c.req.param('slug'))
    return block ? c.json({ block }) : c.json({ error: { code: 'NOT_FOUND', message: 'ブロックが見つかりません' } }, 404)
  })
  return routes
}
```

- [ ] **Step 5: APIテストを通してコミットする**

Run: `npm run test:worker -- test/worker/public-api.test.ts`

Expected: all public API tests PASS。

```bash
git add games/minna-no-monosashi/src/shared games/minna-no-monosashi/src/worker games/minna-no-monosashi/test/worker/public-api.test.ts
git commit -m "feat: expose public question blocks"
```

### Task 5: 匿名回答・重複防止・統計API

**Files:**
- Create: `games/minna-no-monosashi/src/worker/auth/crypto.ts`
- Create: `games/minna-no-monosashi/src/worker/repositories/responseRepository.ts`
- Create: `games/minna-no-monosashi/src/worker/services/responseService.ts`
- Create: `games/minna-no-monosashi/src/worker/routes/responseRoutes.ts`
- Modify: `games/minna-no-monosashi/src/worker/app.ts`
- Test: `games/minna-no-monosashi/test/worker/vote-api.test.ts`

- [ ] **Step 1: 回答APIの失敗テストを書く**

```ts
it('stores one answer, returns results, and makes same-answer retries idempotent', async () => {
  await seedPublishedQuestion(env.DB)
  const request = () => SELF.fetch('https://example.test/api/blocks/daily/questions/q1/responses', {
    method: 'POST', headers: { 'content-type': 'application/json', 'x-device-token': 'device-a' },
    body: JSON.stringify({ optionId: 'o1' }),
  })
  expect((await request()).status).toBe(201)
  expect((await request()).status).toBe(200)
  const row = await env.DB.prepare('SELECT COUNT(*) count FROM responses').first<{ count: number }>()
  expect(row?.count).toBe(1)
})
```

別選択肢への再送が409、下書き・終了済みへの回答が拒否、質問に属さない選択肢が400、未回答状態の結果取得が403、3件を1件ずつ投票した割合が34/33/33になることも同じファイルへ書く。

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:worker -- test/worker/vote-api.test.ts`

Expected: FAIL with route not found。

- [ ] **Step 3: 端末トークンをHMAC化する**

```ts
export async function hmacHex(secret: string, value: string): Promise<string> {
  const encoder = new TextEncoder()
  const key = await crypto.subtle.importKey('raw', encoder.encode(secret), { name: 'HMAC', hash: 'SHA-256' }, false, ['sign'])
  const bytes = new Uint8Array(await crypto.subtle.sign('HMAC', key, encoder.encode(value)))
  return Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('')
}
```

- [ ] **Step 4: 回答repositoryとserviceを実装する**

`ResponseRepository` に `findExisting(questionId, voterHash)`、`insert(response)`、`aggregate(questionId)`、`progress(blockId, voterHash)` を作る。`ResponseService.answer()` は所属関係とblock statusを1queryで確認し、既存回答が同一optionなら200相当、別optionなら `ANSWER_LOCKED`、未回答ならINSERTする。INSERTのunique違反も再読込して同じ分岐へ集約する。

```ts
export type AnswerOutcome = {
  created: boolean
  selectedOptionId: string
  totalResponses: number
  options: { id: string; label: string; count: number; percentage: number }[]
}
```

- [ ] **Step 5: rate limitと回答・結果・進捗routesを実装する**

回答前にHMAC化した端末キーを `VOTE_RATE_LIMITER.limit({ key: voterHash })` へ渡す。失敗時は429 `RATE_LIMITED`。生トークンやIPはログへ出さない。結果APIは回答済みhashをrepositoryで確認してから返し、終了済みブロックの全体結果だけはトークンなしで返す。

- [ ] **Step 6: vote testsを通してコミットする**

Run: `npm run test:worker -- test/worker/vote-api.test.ts`

Expected: all vote, idempotency, visibility and percentage tests PASS。

```bash
git add games/minna-no-monosashi/src/worker games/minna-no-monosashi/test/worker/vote-api.test.ts
git commit -m "feat: record anonymous answers and statistics"
```

### Task 6: 管理者認証・Cookie・CSRF

**Files:**
- Create: `games/minna-no-monosashi/scripts/hash-admin-password.mjs`
- Create: `games/minna-no-monosashi/.dev.vars.example`
- Create: `games/minna-no-monosashi/src/worker/auth/session.ts`
- Create: `games/minna-no-monosashi/src/worker/auth/csrf.ts`
- Create: `games/minna-no-monosashi/src/worker/http/errors.ts`
- Create: `games/minna-no-monosashi/src/worker/http/securityHeaders.ts`
- Create: `games/minna-no-monosashi/src/worker/routes/adminRoutes.ts`
- Modify: `games/minna-no-monosashi/src/worker/app.ts`
- Test: `games/minna-no-monosashi/test/worker/security.test.ts`

- [ ] **Step 1: 認証・CSRFの失敗テストを書く**

未ログインの `/api/admin/session` が401、不正passwordが401、正しいpasswordで `HttpOnly; Secure; SameSite=Strict` CookieとCSRF tokenを受け取れること、Origin不一致とCSRF不一致のPOSTが403になることを検証する。

```ts
expect(login.headers.get('set-cookie')).toMatch(/HttpOnly.*Secure.*SameSite=Strict/)
expect(await login.json()).toEqual({ authenticated: true, csrfToken: expect.any(String) })
```

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:worker -- test/worker/security.test.ts`

Expected: FAIL because auth routes do not exist。

- [ ] **Step 3: PBKDF2 hash生成と検証を実装する**

hashはアルゴリズム名、反復回数、saltのBase64、32bytes hashのBase64を `$` で連結する。開発値の完全な例はStep 5の `ADMIN_PASSWORD_HASH` とする。生成scriptは `crypto.randomBytes(16)` と `pbkdf2Sync(password, salt, 210000, 32, 'sha256')` を使い、passwordを引数または非表示promptから受け取る。Worker側はWeb Crypto PBKDF2で同じ32bytesを導出し、固定時間比較する。

- [ ] **Step 4: 署名sessionとCSRFを実装する**

session payloadはUnix秒の有効期限 `exp` と128bit乱数の16進文字列 `csrf` を持ち、base64url payloadとHMAC-SHA256署名を `.` で連結する。有効期限は8時間。状態変更routeではsession、`Origin === new URL(request.url).origin`、`X-CSRF-Token === payload.csrf` の3条件を要求する。

`adminRoutes.ts` にはこの時点でlogin、logout、sessionの3routeだけを実装し、`app.route('/api/admin', adminRoutes())` で接続する。loginは `LOGIN_RATE_LIMITER` を、Secret付きHMACで不可逆化した `cf-connecting-ip` または固定fallback文字列に対して適用する。生IPはDB・Cookie・ログへ残さない。Task 7で同じrouterへブロック管理routeを追加する。

- [ ] **Step 5: 開発用秘密値例を追加する**

```dotenv
ADMIN_PASSWORD_HASH=pbkdf2-sha256$210000$bWlubmEtbm8tbW9ub3Nhc2hpLWRldg==$NZZzZOqqD6oGmwTJ86r/pFEoY5sZ2UWZclDfdeOJLOM=
SESSION_SECRET=local-only-session-secret-change-before-deploy
VOTER_HASH_SECRET=local-only-voter-secret-change-before-deploy
```

開発用passwordが `dev-admin` であることをREADMEへ明記し、本番ではこの値を使わない。

- [ ] **Step 6: security testsを通してコミットする**

Run: `npm run test:worker -- test/worker/security.test.ts`

Expected: all authentication, cookie, Origin and CSRF tests PASS。

```bash
git add games/minna-no-monosashi/scripts games/minna-no-monosashi/.dev.vars.example games/minna-no-monosashi/src/worker games/minna-no-monosashi/test/worker/security.test.ts
git commit -m "feat: secure administrator sessions"
```

### Task 7: 管理APIとブロックライフサイクル

**Files:**
- Modify: `games/minna-no-monosashi/src/worker/repositories/blockRepository.ts`
- Modify: `games/minna-no-monosashi/src/worker/services/blockService.ts`
- Modify: `games/minna-no-monosashi/src/worker/routes/adminRoutes.ts`
- Modify: `games/minna-no-monosashi/src/worker/app.ts`
- Test: `games/minna-no-monosashi/test/worker/admin-api.test.ts`

- [ ] **Step 1: 管理APIの失敗テストを書く**

認証済みrequest helperを作り、下書き作成・取得・更新・削除、公開、終了、複製を検証する。10問は公開成功、0問/11問/重複選択肢は公開失敗、公開後PUT/DELETEは409、終了後の再公開は409、複製は新しいdraftと一意slugを返すことをassertする。

```ts
expect(publish.status).toBe(200)
expect((await publish.json()).block.status).toBe('published')
expect((await SELF.fetch(updatePublishedRequest)).status).toBe(409)
```

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:worker -- test/worker/admin-api.test.ts`

Expected: FAIL because admin block routes do not exist。

- [ ] **Step 3: transaction境界を持つrepository操作を実装する**

下書き保存はblockをupsertし、既存questions/optionsを削除して、positionを0から振り直したINSERT群を `DB.batch()` で実行する。公開は現在の全内容を読み、`validateDraft()` が空のときだけ `status='published', published_at=?` を更新する。終了はpublishedだけをclosedにする。複製はblock/questions/optionsへ新IDを振り、元slugが `daily-boundaries` なら `daily-boundaries-copy`、競合時は `daily-boundaries-copy-2` 以降にする規則を全slugへ適用する。

- [ ] **Step 4: 管理routesを実装する**

```text
POST   /api/admin/login
POST   /api/admin/logout
GET    /api/admin/session
GET    /api/admin/blocks
POST   /api/admin/blocks
GET    /api/admin/blocks/:id
PUT    /api/admin/blocks/:id
DELETE /api/admin/blocks/:id
POST   /api/admin/blocks/:id/publish
POST   /api/admin/blocks/:id/close
POST   /api/admin/blocks/:id/clone
GET    /api/admin/blocks/:id/results
```

全状態変更routeへ認証・Origin・CSRF middlewareを適用し、domainの `ValidationIssue[]` を422 `VALIDATION_FAILED` のdetailsとして返す。

- [ ] **Step 5: admin API testsと全Worker testsを通す**

Run: `npm run test:worker`

Expected: public, vote, security, admin suites all PASS。

- [ ] **Step 6: コミットする**

```bash
git add games/minna-no-monosashi/src/worker games/minna-no-monosashi/test/worker/admin-api.test.ts
git commit -m "feat: manage question block lifecycle"
```

### Task 8: 参加者トークン、API client、公開ルーティング

**Files:**
- Create: `games/minna-no-monosashi/src/client/deviceToken.ts`
- Test: `games/minna-no-monosashi/src/client/deviceToken.test.ts`
- Create: `games/minna-no-monosashi/src/client/api/client.ts`
- Create: `games/minna-no-monosashi/src/client/components/AppLayout.tsx`
- Create: `games/minna-no-monosashi/src/client/components/AsyncState.tsx`
- Modify: `games/minna-no-monosashi/src/app/App.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/HomePage.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/BlockIntroPage.tsx`

- [ ] **Step 1: 端末トークンの失敗テストを書く**

```ts
it('reuses a persisted token and falls back to a tab token when localStorage throws', () => {
  const values = new Map<string, string>()
  const storage = { getItem: (key: string) => values.get(key) ?? null, setItem: (key: string, value: string) => { values.set(key, value) } }
  expect(getDeviceIdentity(storage).token).toBe(getDeviceIdentity(storage).token)
  const broken = { getItem: () => { throw new Error('blocked') }, setItem: () => { throw new Error('blocked') } }
  expect(getDeviceIdentity(broken).persistent).toBe(false)
})
```

- [ ] **Step 2: 失敗を確認し、token生成を実装する**

Run: `npm run test:unit -- src/client/deviceToken.test.ts`

Expected: FAIL before implementation。

`crypto.getRandomValues(new Uint8Array(24))` をbase64url化し、key `minna-no-monosashi:device-token:v1` へ保存する。module内の一時tokenにfallbackし `{ token, persistent }` を返す。

- [ ] **Step 3: 型付きAPI clientを実装する**

```ts
export class ApiError extends Error {
  constructor(readonly status: number, readonly code: string, message: string, readonly details?: unknown) { super(message) }
}

export async function apiRequest<T>(path: string, init: RequestInit = {}, deviceToken?: string): Promise<T> {
  const response = await fetch(path, { ...init, headers: { 'content-type': 'application/json', ...init.headers, ...(deviceToken ? { 'x-device-token': deviceToken } : {}) } })
  const body = await response.json()
  if (!response.ok) throw new ApiError(response.status, body.error.code, body.error.message, body.error.details)
  return body as T
}
```

- [ ] **Step 4: 公開routeと一覧・導入画面を実装する**

`BrowserRouter` 配下に設計書どおりの公開routeとadmin routeを登録する。Homeは公開中と終了済みを分け、BlockIntroは質問数、総参加者数、「はじめる」または「続きから」を表示する。ロード中・404・通信失敗は `AsyncState` に集約する。

- [ ] **Step 5: unit tests、型、buildを通してコミットする**

Run: `npm run test:unit && npm run typecheck && npm run build`

Expected: all current UI/domain tests PASS and build succeeds。

```bash
git add games/minna-no-monosashi/src/client games/minna-no-monosashi/src/app
git commit -m "feat: add public block discovery"
```

### Task 9: 質問・回答結果・再開・振り返りUI

**Files:**
- Test: `games/minna-no-monosashi/src/client/pages/QuestionPage.test.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/QuestionPage.tsx`
- Create: `games/minna-no-monosashi/src/client/components/QuestionForm.tsx`
- Create: `games/minna-no-monosashi/src/client/components/ResultChart.tsx`
- Test: `games/minna-no-monosashi/src/client/components/ResultChart.test.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/QuestionResultPage.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/SummaryPage.tsx`
- Create: `games/minna-no-monosashi/src/client/pages/ClosedResultsPage.tsx`
- Modify: `games/minna-no-monosashi/src/app/App.tsx`

- [ ] **Step 1: 単一回答と送信状態の失敗テストを書く**

```tsx
it('submits the selected option once and disables controls while pending', async () => {
  const question = { id: 'q1', prompt: '朝早いのは？', position: 0, options: [{ id: 'o1', label: '6時', position: 0 }, { id: 'o2', label: '7時', position: 1 }] }
  let resolveAnswer!: () => void
  const submitAnswer = vi.fn(() => new Promise<void>((resolve) => { resolveAnswer = resolve }))
  render(<QuestionForm question={question} onSubmit={submitAnswer} />)
  await user.click(screen.getByRole('radio', { name: '7時' }))
  await user.click(screen.getByRole('button', { name: 'この答えにする' }))
  expect(submitAnswer).toHaveBeenCalledWith('o2')
  expect(screen.getByRole('button', { name: '送信中' })).toBeDisabled()
  resolveAnswer()
})
```

ネットワーク失敗時に同じ選択を保持して再送でき、`ANSWER_LOCKED` 時に既存結果へ移動することもテストする。

- [ ] **Step 2: 失敗を確認して質問画面を実装する**

Run: `npm run test:unit -- src/client/pages/QuestionPage.test.tsx`

Expected: FAIL before component exists。

radio group、確認button、`aria-live` statusを実装する。送信成功時はresult routeへnavigateし、ローカルだけで回答済み扱いにせず、必ずAPIレスポンスを使う。

- [ ] **Step 3: ResultChartの失敗テストと実装を行う**

人数、割合、棒の `aria-label`、自分の回答を示す「あなた」の文字を検証する。棒グラフは `width: ${percentage}%` だけに依存せず、テキストで全値を読めるようにする。

- [ ] **Step 4: 結果・summary・closed resultsを実装する**

QuestionResultは回答APIの結果またはGET結果を表示し、次の未回答質問へ進む。Summaryはprogress APIが全問回答済みの場合だけ表示し、自分の選択を含む全結果を並べる。ClosedResultsはdevice tokenなしで全質問を表示する。

- [ ] **Step 5: 途中再開を実装する**

BlockIntroでprogress APIから `answeredQuestionIds` と `nextQuestionPosition` を取得し、次の未回答へnavigateする。全問回答済みならsummaryへ進む。一時token利用時は「再読み込みすると回答状況を復元できません」を開始前に表示する。

- [ ] **Step 6: UI testsとbuildを通してコミットする**

Run: `npm run test:unit && npm run build`

Expected: question, chart, token and app tests PASS; build succeeds。

```bash
git add games/minna-no-monosashi/src/client games/minna-no-monosashi/src/app
git commit -m "feat: add anonymous question experience"
```

### Task 10: 管理画面UI

**Files:**
- Create: `games/minna-no-monosashi/src/client/admin/AdminLoginPage.tsx`
- Create: `games/minna-no-monosashi/src/client/admin/AdminDashboardPage.tsx`
- Test: `games/minna-no-monosashi/src/client/admin/BlockEditorPage.test.tsx`
- Create: `games/minna-no-monosashi/src/client/admin/BlockEditorPage.tsx`
- Create: `games/minna-no-monosashi/src/client/admin/AdminResultsPage.tsx`
- Modify: `games/minna-no-monosashi/src/client/api/client.ts`
- Modify: `games/minna-no-monosashi/src/app/App.tsx`

- [ ] **Step 1: 可変質問・選択肢editorの失敗テストを書く**

質問追加が10問で無効、2個未満になる選択肢削除が無効、質問・選択肢を上下へ並べ替えられる、APIのfield issueを該当入力へ表示する、published blockは全入力がread-onlyになることを検証する。

```tsx
expect(screen.getAllByRole('group', { name: /質問/ })).toHaveLength(10)
expect(screen.getByRole('button', { name: '質問を追加' })).toBeDisabled()
expect(screen.getByText('選択肢は重複しない1〜80文字を2個以上入力してください')).toBeVisible()
```

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:unit -- src/client/admin/BlockEditorPage.test.tsx`

Expected: FAIL before admin components exist。

- [ ] **Step 3: loginとsession維持を実装する**

login POST後にCSRF tokenをmemoryとsessionStorageへ保持する。管理API clientは `credentials: 'same-origin'` と `X-CSRF-Token` を付ける。401時は編集中draftをsessionStorageへ一時保存してloginへ戻し、再ログイン後に復元確認を出す。

- [ ] **Step 4: dashboardとeditorを実装する**

Dashboardはdraft/published/closedの3区分と参加者数を表示する。Editorはtitle/description/slug、1〜10問、2個以上の選択肢、上下buttonによる並べ替え、下書き保存、preview、公開を提供する。published/closedでは編集を隠し、複製・結果・終了だけを状態に応じて出す。

- [ ] **Step 5: admin resultsを実装する**

質問ごとのResultChartとブロック総参加者数を表示し、回答者を識別できる値は画面やdownloadへ出さない。

- [ ] **Step 6: admin UI testsとbuildを通してコミットする**

Run: `npm run test:unit && npm run build`

Expected: all admin and public UI tests PASS; build succeeds。

```bash
git add games/minna-no-monosashi/src/client games/minna-no-monosashi/src/app
git commit -m "feat: add question block administration"
```

### Task 11: 独自ビジュアル、レスポンシブ、アクセシビリティ

**Files:**
- Modify: `games/minna-no-monosashi/src/client/styles.css`
- Modify: `games/minna-no-monosashi/src/client/components/AppLayout.tsx`
- Modify: `games/minna-no-monosashi/src/client/components/ResultChart.tsx`
- Modify: `games/minna-no-monosashi/index.html`
- Create: `games/minna-no-monosashi/public/favicon.svg`

- [ ] **Step 1: モバイル・キーボード受け入れ条件をテストへ追加する**

QuestionPage testへradioのkeyboard選択、focus-visible、結果のテキスト値を追加する。App testへskip linkと一意なmain landmarkを追加する。

- [ ] **Step 2: 失敗を確認する**

Run: `npm run test:unit`

Expected: accessibility assertions FAIL before markup/style updates。

- [ ] **Step 3: 承認済みモックのvisual tokenをCSSへ実装する**

```css
:root {
  color: #27231d; background: #fffaf1;
  font-family: Inter, "Hiragino Sans", "Yu Gothic", sans-serif;
  --accent: #e85d3b; --ink: #27231d; --paper: #fffaf1; --line: #d9d4ca;
}
.question-title { font-family: "Hiragino Mincho ProN", "Yu Mincho", serif; font-size: clamp(1.8rem, 7vw, 3.5rem); line-height: 1.42; }
.choice { min-height: 52px; border: 1px solid var(--line); border-radius: 14px; }
.choice:focus-within, button:focus-visible, a:focus-visible { outline: 3px solid #1a6bb4; outline-offset: 3px; }
@media (prefers-reduced-motion: reduce) { *, *::before, *::after { scroll-behavior: auto !important; transition-duration: 0.01ms !important; } }
```

- [ ] **Step 4: responsive layoutと非色依存表示を仕上げる**

参加画面は最大680pxの1カラム、管理editorは1100px以上で質問一覧と編集領域の2カラム、それ未満は1カラムとする。ResultChartはaccent色に加え「あなた」badgeとcheck iconを持ち、タップ対象を44px以上にする。

- [ ] **Step 5: unit testsとbuildを通してコミットする**

Run: `npm run test:unit && npm run build`

Expected: accessibility tests PASS and build succeeds。

```bash
git add games/minna-no-monosashi/src/client games/minna-no-monosashi/index.html games/minna-no-monosashi/public
git commit -m "feat: polish responsive exhibition experience"
```

### Task 12: サンプル10問とローカルE2E

**Files:**
- Create: `games/minna-no-monosashi/seed/sample-block.sql`
- Create: `games/minna-no-monosashi/playwright.config.ts`
- Create: `games/minna-no-monosashi/e2e/app.spec.ts`
- Create: `games/minna-no-monosashi/.gitignore`
- Create: `games/minna-no-monosashi/README.md`

- [ ] **Step 1: 再実行可能なseed SQLを書く**

block IDを `sample-daily-boundaries`、slugを `daily-boundaries` に固定し、設計書の「日常の境界線」10問と40選択肢を `INSERT OR IGNORE` する。全question/option IDも固定し、2回実行して件数が増えないことをローカルD1 queryで確認する。

Run: `npm run db:migrate:local && npm run db:seed:local && npm run db:seed:local && npx wrangler d1 execute DB --local --command="SELECT COUNT(*) AS count FROM questions WHERE block_id='sample-daily-boundaries'"`

Expected: `count` is 10。

- [ ] **Step 2: Playwright設定と最初のE2E失敗テストを書く**

```ts
export default defineConfig({
  testDir: './e2e',
  webServer: { command: 'npm run db:migrate:local && npm run db:seed:local && npm run dev -- --host 127.0.0.1', port: 5173, reuseExistingServer: false },
  projects: [
    { name: 'mobile', use: { viewport: { width: 390, height: 844 } } },
    { name: 'desktop', use: { viewport: { width: 1440, height: 900 } } },
  ],
})
```

実行前に `.dev.vars.example` を `.dev.vars` へコピーし、Playwrightでは開発用password `dev-admin` を使う。各projectは一意slugを使い、前回実行のD1データが残っていても衝突しないようにする。

E2Eは管理者login、10問draft作成、公開、匿名10回答、各問結果、summary、同一browser再回答不可、別contextの集計反映、管理者終了、新規contextから回答拒否、closed results公開までを一つのserial scenarioで検証する。

- [ ] **Step 3: E2Eを実行して不足を修正する**

Run: `npm run test:e2e`

Expected: mobile and desktop scenarios PASS。失敗した場合はテストを弱めず、API/UI/selectorを修正して再実行する。

- [ ] **Step 4: READMEへ再現可能な手順を書く**

READMEにNode要件、install、`.dev.vars.example` copy、local migration、seed、dev、test、build、remote D1 create/migrate/seed、3 secrets設定、deploy、公開後検証を順に記載する。端末単位の制限とlocalStorage削除で再回答できる制約も明記する。

- [ ] **Step 5: 全ローカル検証を通す**

Run: `npm test && npm run test:e2e && npm run typecheck && npm run build`

Expected: unit、Worker integration、mobile/desktop E2E、typecheck、buildが全てexit 0。

- [ ] **Step 6: コミットする**

```bash
git add games/minna-no-monosashi
git commit -m "test: verify complete question block journey"
```

### Task 13: Cloudflare本番作成・デプロイ・外部検証

**Files:**
- Modify: `games/minna-no-monosashi/wrangler.jsonc`
- Modify: `games/minna-no-monosashi/README.md`

- [ ] **Step 1: Cloudflare認証状態を読み取り確認する**

Run: `npx wrangler whoami`

Expected: deploy先accountが表示される。未認証ならユーザーへloginを依頼し、このtaskだけを保留する。

- [ ] **Step 2: 本番D1を作成し、実IDを設定する**

Run: `npx wrangler d1 create minna-no-monosashi`

Expected: UUID形式のdatabase_idが返る。`wrangler.jsonc` のzero UUIDをその実IDへ置き換える。

- [ ] **Step 3: 強い本番secretを設定する**

`node scripts/hash-admin-password.mjs` でユーザーが入力した本番passwordのhashを生成し、次を実行する。平文passwordやsecret値をterminal出力、git、READMEへ残さない。

```bash
npx wrangler secret put ADMIN_PASSWORD_HASH
npx wrangler secret put SESSION_SECRET
npx wrangler secret put VOTER_HASH_SECRET
```

Expected: three secrets report success。

- [ ] **Step 4: 本番migration、seed、deployを行う**

Run: `npm run db:migrate:remote && npm run db:seed:remote && npm run deploy`

Expected: migration and seed succeed; Wrangler returns an HTTPS workers.dev or custom-domain URL。

- [ ] **Step 5: 公開URLを2つの独立browser contextで検証する**

Wranglerが返した公開URLを `BASE_URL` 環境変数へ設定したPlaywright実行、または実ブラウザで、context AとBが同じ質問へ異なる回答を行い、両方の集計が合計2人を示すこと、Aが再回答できないこと、管理画面で同じ集計を確認できることを検証する。公開URLをREADMEの「Deployed instance」へ記載する。

- [ ] **Step 6: 最終検証とcompletion auditを行う**

Run: `npm test && npm run test:e2e && npm run typecheck && npm run build && git status --short`

Expected: all checks pass and status contains only intentional `wrangler.jsonc` / README changes。

設計書の完成条件14項目を1件ずつ、test名、source file、公開URLのいずれかへ対応付ける。対応証拠がない項目は完了扱いにしない。

- [ ] **Step 7: 本番情報をコミットする**

```bash
git add games/minna-no-monosashi/wrangler.jsonc games/minna-no-monosashi/README.md
git commit -m "chore: deploy minna no monosashi"
```

## 参考資料

- 設計書：`docs/superpowers/specs/2026-07-20-minna-no-monosashi-design.md`
- Cloudflare React SPA + API tutorial：<https://developers.cloudflare.com/workers/vite-plugin/tutorial/>
- Cloudflare Workers Static Assets full-stack routing：<https://developers.cloudflare.com/workers/static-assets/routing/full-stack-application/>
- Cloudflare D1 getting started：<https://developers.cloudflare.com/d1/get-started/>
- Cloudflare Workers Vitest integration：<https://developers.cloudflare.com/workers/testing/vitest-integration/>
- Cloudflare Workers Rate Limiting binding：<https://developers.cloudflare.com/workers/runtime-apis/bindings/rate-limit/>
