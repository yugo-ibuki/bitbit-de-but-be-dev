# Cloudflare Workers Sandbox Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build and deploy an isolated Cloudflare Workers sandbox that demonstrates Hono, Queues, durable Workflows, retries, DLQ behavior, idempotency, and explicit fan-out.

**Architecture:** One Worker script exposes a Hono fetch handler, routes two Queue consumers, and exports a `WorkflowEntrypoint`. Pure validation and calculation modules stay independent of Cloudflare bindings; the API, consumer, and Workflow boundaries inject or receive bindings and use a deterministic job ID as the Workflow instance ID.

**Tech Stack:** TypeScript, Bun, Hono, Cloudflare Workers, Cloudflare Queues, Cloudflare Workflows, Wrangler JSONC, Vitest, Cloudflare Workers Vitest integration

---

### Task 1: Scaffold the independent Bun and Wrangler project

**Files:**
- Create: `cloudflare/workers-sandbox/package.json`
- Create: `cloudflare/workers-sandbox/tsconfig.json`
- Create: `cloudflare/workers-sandbox/vitest.config.ts`
- Create: `cloudflare/workers-sandbox/wrangler.jsonc`
- Create: `cloudflare/workers-sandbox/.gitignore`

- [ ] **Step 1: Create package metadata and scripts**

Define a private ESM package with Bun as package manager. Add `hono` as a runtime dependency and `@cloudflare/vitest-pool-workers`, `@cloudflare/workers-types`, `typescript`, `vitest`, and `wrangler` as development dependencies. Add these scripts:

```json
{
  "dev": "wrangler dev",
  "test": "vitest run",
  "typecheck": "tsc --noEmit",
  "cf-typegen": "wrangler types",
  "cf-typegen:check": "wrangler types --check",
  "deploy": "wrangler deploy",
  "deploy:dry-run": "wrangler deploy --dry-run",
  "check:startup": "wrangler check startup"
}
```

- [ ] **Step 2: Configure TypeScript and Vitest**

Use `ES2022`, `ESNext`, bundler resolution, strict mode, no emit, and the generated `worker-configuration.d.ts`. Configure Workers Vitest with `wrangler.configPath: "./wrangler.jsonc"`.

- [ ] **Step 3: Configure sandbox bindings**

Create a `wrangler.jsonc` with compatibility date `2026-07-26`, `nodejs_compat`, observability, producers `JOB_QUEUE` and `AUDIT_QUEUE`, consumers for `workers-sandbox-jobs` and `workers-sandbox-audit`, DLQ `workers-sandbox-jobs-dlq`, retry delay, retry limit, and Workflow binding `JOB_WORKFLOW`. Do not add an account ID.

- [ ] **Step 4: Install dependencies**

Run:

```bash
bun install
```

Expected: `bun.lock` is created and install exits 0.

- [ ] **Step 5: Commit the scaffold**

```bash
git add cloudflare/workers-sandbox
git commit -m "chore: scaffold Cloudflare Workers sandbox"
```

### Task 2: Implement validated job transformations with TDD

**Files:**
- Create: `cloudflare/workers-sandbox/test/job.test.ts`
- Create: `cloudflare/workers-sandbox/src/job.ts`
- Create: `cloudflare/workers-sandbox/src/messages.ts`

- [ ] **Step 1: Write failing calculation and validation tests**

Cover these exact cases:

```ts
expect(transformInput({ id: "customer-42", values: [10, 20, 30] })).toEqual({
  targetId: "customer-42",
  values: [10, 20, 30],
  count: 3,
  sum: 60,
});
expect(calculateResult(transformed, "average")).toEqual({
  targetId: "customer-42",
  operation: "average",
  result: 20,
});
expect(parseJobRequest({ target: { id: "", values: [] }, operation: "sum" }).success).toBe(false);
expect(parseJobRequest({ target: { id: "x", values: [1] }, operation: "median" }).success).toBe(false);
```

Also cover `sum`, `max`, non-finite numbers, more than 100 values, target IDs over 100 characters, and default `fanout`/`failStepBOnce` values.

- [ ] **Step 2: Run tests and confirm RED**

Run:

```bash
bun run test test/job.test.ts
```

Expected: FAIL because `src/job.ts` and `src/messages.ts` do not exist.

- [ ] **Step 3: Implement minimal pure functions**

Implement:

```ts
export type Operation = "sum" | "average" | "max";
export type JobTarget = { id: string; values: number[] };
export type TransformedInput = {
  targetId: string;
  values: number[];
  count: number;
  sum: number;
};
export type JobResult = {
  targetId: string;
  operation: Operation;
  result: number;
};
export function transformInput(target: JobTarget): TransformedInput;
export function calculateResult(input: TransformedInput, operation: Operation): JobResult;
```

Implement `parseJobRequest(value: unknown)` as a discriminated result. Accept only a non-empty target ID of at most 100 characters, 1–100 finite numeric values, a supported operation, and optional boolean flags.

- [ ] **Step 4: Run tests and confirm GREEN**

Run:

```bash
bun run test test/job.test.ts
```

Expected: all job tests pass.

- [ ] **Step 5: Commit the domain behavior**

```bash
git add cloudflare/workers-sandbox/src/job.ts cloudflare/workers-sandbox/src/messages.ts cloudflare/workers-sandbox/test/job.test.ts
git commit -m "feat: add sandbox job validation and calculations"
```

### Task 3: Implement the Hono API with TDD

**Files:**
- Create: `cloudflare/workers-sandbox/test/api.test.ts`
- Create: `cloudflare/workers-sandbox/src/api.ts`

- [ ] **Step 1: Write failing API tests**

Create fake Queue and Workflow bindings and assert:

```ts
const health = await app.request("/health", {}, env);
expect(health.status).toBe(200);
expect(await health.json()).toEqual({
  status: "ok",
  service: "workers-sandbox-api",
});
```

For `POST /jobs`, stub `crypto.randomUUID()` through an injected `createJobId` dependency and assert status `202`, `JOB_QUEUE.send()` receives a complete `JobMessage`, audit send occurs only with `fanout: true`, and invalid input returns `400`. For `GET /jobs/:jobId`, assert status output is returned and missing instances return `404`.

- [ ] **Step 2: Run tests and confirm RED**

Run:

```bash
bun run test test/api.test.ts
```

Expected: FAIL because `src/api.ts` does not exist.

- [ ] **Step 3: Implement the Hono app factory**

Export:

```ts
export type ApiBindings = {
  JOB_QUEUE: Queue<JobMessage>;
  AUDIT_QUEUE: Queue<AuditMessage>;
  JOB_WORKFLOW: Workflow<JobMessage>;
};

export function createApp(options?: {
  createJobId?: () => string;
  now?: () => string;
}): Hono<{ Bindings: ApiBindings }>;
```

Return JSON errors in the form:

```json
{
  "error": {
    "code": "INVALID_JOB",
    "message": "target.id must be a non-empty string"
  }
}
```

Queue the main message first. If explicit fan-out is requested, send an audit message containing `eventType: "job.submitted"`, job ID, target ID, and submitted time.

- [ ] **Step 4: Run tests and confirm GREEN**

Run:

```bash
bun run test test/api.test.ts
```

Expected: all API tests pass.

- [ ] **Step 5: Commit the API**

```bash
git add cloudflare/workers-sandbox/src/api.ts cloudflare/workers-sandbox/test/api.test.ts
git commit -m "feat: add Hono job API"
```

### Task 4: Implement idempotent Queue consumption with TDD

**Files:**
- Create: `cloudflare/workers-sandbox/test/consumer.test.ts`
- Create: `cloudflare/workers-sandbox/src/consumer.ts`

- [ ] **Step 1: Write failing Consumer tests**

Use simple fake messages with `ack` and `retry` counters. Assert:

```ts
await processJobMessage(message, workflow);
expect(workflow.createCalls).toHaveLength(1);
expect(message.ackCalls).toBe(1);
expect(message.retryCalls).toBe(0);
```

Also assert an existing Workflow skips `create`, a create race followed by successful `get` is acked, an unresolved create error retries, audit messages log and ack, and an unknown queue throws.

- [ ] **Step 2: Run tests and confirm RED**

Run:

```bash
bun run test test/consumer.test.ts
```

Expected: FAIL because `src/consumer.ts` does not exist.

- [ ] **Step 3: Implement per-message handlers**

Export:

```ts
export async function processJobMessage(
  message: Message<JobMessage>,
  workflow: Workflow<JobMessage>,
): Promise<void>;

export async function processAuditMessage(
  message: Message<AuditMessage>,
  logger?: Pick<Console, "log">,
): Promise<void>;

export async function consumeQueue(
  batch: MessageBatch<JobMessage | AuditMessage>,
  env: ConsumerBindings,
): Promise<void>;
```

Use `workflow.get(jobId)` for the first existence check. If missing, call `create`. If `create` throws, call `get` again; ack if it now exists and otherwise call `message.retry()`. Await every promise.

- [ ] **Step 4: Run tests and confirm GREEN**

Run:

```bash
bun run test test/consumer.test.ts
```

Expected: all Consumer tests pass.

- [ ] **Step 5: Commit the Consumer**

```bash
git add cloudflare/workers-sandbox/src/consumer.ts cloudflare/workers-sandbox/test/consumer.test.ts
git commit -m "feat: add idempotent queue consumer"
```

### Task 5: Implement the durable Workflow and Worker entrypoint with TDD

**Files:**
- Create: `cloudflare/workers-sandbox/test/workflow.test.ts`
- Create: `cloudflare/workers-sandbox/test/worker.test.ts`
- Create: `cloudflare/workers-sandbox/src/workflow.ts`
- Create: `cloudflare/workers-sandbox/src/index.ts`
- Generate: `cloudflare/workers-sandbox/worker-configuration.d.ts`

- [ ] **Step 1: Write failing Workflow behavior tests**

Use the Cloudflare Workflows introspection API to create an instance with known values. Assert `transform-input` returns count and sum, `calculate-result` receives the persisted result and returns the expected operation result, and `failStepBOnce: true` makes the first Step B attempt fail before a later attempt succeeds.

- [ ] **Step 2: Write failing Worker entrypoint tests**

Assert the default export handles `/health` through Hono and the queue export delegates a known queue batch to the Consumer.

- [ ] **Step 3: Run tests and confirm RED**

Run:

```bash
bun run test test/workflow.test.ts test/worker.test.ts
```

Expected: FAIL because `src/workflow.ts` and `src/index.ts` do not exist.

- [ ] **Step 4: Implement the Workflow class**

Extend `WorkflowEntrypoint<Env, JobMessage>` and implement:

```ts
const transformed = await step.do("transform-input", async (ctx) => {
  console.log(JSON.stringify({
    event: "workflow.step",
    jobId: event.payload.jobId,
    step: ctx.step.name,
    attempt: ctx.attempt,
  }));
  return transformInput(event.payload.target);
});

return step.do(
  "calculate-result",
  {
    retries: { limit: 3, delay: "1 second", backoff: "constant" },
    timeout: "30 seconds",
  },
  async (ctx) => {
    if (event.payload.failStepBOnce && ctx.attempt === 1) {
      throw new Error("Intentional first-attempt failure");
    }
    return calculateResult(transformed, event.payload.operation);
  },
);
```

- [ ] **Step 5: Implement the Worker entrypoint**

Create the Hono app once at module scope because it contains no request-scoped mutable state. Export `JobWorkflow`, and export a default object with awaited `fetch` and `queue` methods:

```ts
export default {
  fetch(request, env, ctx) {
    return app.fetch(request, env, ctx);
  },
  queue(batch, env) {
    return consumeQueue(batch, env);
  },
} satisfies ExportedHandler<Env>;
```

- [ ] **Step 6: Generate types**

Run:

```bash
bun run cf-typegen
```

Expected: `worker-configuration.d.ts` contains `JOB_QUEUE`, `AUDIT_QUEUE`, and `JOB_WORKFLOW`.

- [ ] **Step 7: Run tests and confirm GREEN**

Run:

```bash
bun run test test/workflow.test.ts test/worker.test.ts
```

Expected: Workflow and entrypoint tests pass.

- [ ] **Step 8: Commit the Workflow and entrypoint**

```bash
git add cloudflare/workers-sandbox/src/workflow.ts cloudflare/workers-sandbox/src/index.ts cloudflare/workers-sandbox/test/workflow.test.ts cloudflare/workers-sandbox/test/worker.test.ts cloudflare/workers-sandbox/worker-configuration.d.ts
git commit -m "feat: connect queues to durable workflow"
```

### Task 6: Document and verify the complete sandbox

**Files:**
- Create: `cloudflare/workers-sandbox/README.md`

- [ ] **Step 1: Write the README**

Document prerequisites, Bun setup, `wrangler dev`, sample curl commands, Queue and DLQ creation, Workflow triggering and inspection, deploy commands, observability, intentional retry flags, explicit fan-out, and the Google Cloud comparison table. State that one push-based Consumer Worker is associated with each Queue, while Worker invocations scale horizontally and are not Pub/Sub subscribers.

Use shell examples that read the account ID from the environment:

```bash
export CLOUDFLARE_ACCOUNT_ID="<cloudflare-sandbox account ID>"
bunx wrangler whoami
bunx wrangler queues create workers-sandbox-jobs
bunx wrangler queues create workers-sandbox-jobs-dlq
bunx wrangler queues create workers-sandbox-audit
bun run deploy
```

- [ ] **Step 2: Run the full automated verification**

Run:

```bash
bun run test
bun run typecheck
bun run cf-typegen:check
bun run deploy:dry-run
bun run check:startup
```

Expected: all commands exit 0 with no test failures or TypeScript errors.

- [ ] **Step 3: Run the local flow**

Start:

```bash
bun run dev
```

Then submit:

```bash
curl -s http://localhost:8787/health
curl -s -X POST http://localhost:8787/jobs \
  -H 'content-type: application/json' \
  -d '{"target":{"id":"customer-42","values":[10,20,30]},"operation":"average","fanout":true,"failStepBOnce":true}'
curl -s http://localhost:8787/jobs/<jobId>
```

Expected: health is `ok`, POST returns `202`, Queue logs show main and audit processing, Workflow logs show Step B attempts 1 and 2, and the final status output contains result `20`.

- [ ] **Step 4: Commit the documentation**

```bash
git add cloudflare/workers-sandbox/README.md
git commit -m "docs: add Workers sandbox runbook"
```

### Task 7: Deploy only to the cloudflare-sandbox account

**Files:**
- No repository changes expected.

- [ ] **Step 1: Resolve and verify the target account**

Run `bunx wrangler whoami` and compare the selected account ID with the Cloudflare Dashboard entry named `cloudflare-sandbox`. Export that ID as `CLOUDFLARE_ACCOUNT_ID` only in the current shell. Stop if the name does not match.

- [ ] **Step 2: Create sandbox Queues**

Run the three `wrangler queues create` commands from the README. If a resource already exists, confirm its exact account and name instead of deleting or replacing it.

- [ ] **Step 3: Deploy the Worker and Workflow**

Run:

```bash
bun run deploy
```

Expected: deploy output lists `workers-sandbox-api`, both Queue producer bindings, both Queue consumers, and `workers-sandbox-job-workflow`.

- [ ] **Step 4: Verify the deployed flow**

Call the deployed `/health`, submit a job with intentional Step B retry, poll `/jobs/:jobId`, and inspect Queue and Workflow state in the `cloudflare-sandbox` Dashboard. Confirm no sandbox resources were created in `Y.ibuki91@gmail.com's Account`.

- [ ] **Step 5: Record final verification evidence**

Run `git status --short`, the full automated verification commands again, and the deployed health/job requests. Report exact resource names, Worker URL, job ID, Workflow status, and any Cloudflare limitations observed.
