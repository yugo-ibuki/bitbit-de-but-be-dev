import { describe, expect, it } from "vitest";
import {
  createApp,
  type ApiBindings,
  type WorkflowInstanceHandle,
} from "../src/api";
import type { AuditMessage, JobMessage } from "../src/messages";

class FakeQueue<T> {
  readonly sent: T[] = [];

  async send(body: T): Promise<void> {
    this.sent.push(body);
  }
}

class FakeWorkflow {
  readonly instances = new Map<string, WorkflowInstanceHandle>();

  async get(id: string): Promise<WorkflowInstanceHandle> {
    const instance = this.instances.get(id);
    if (!instance) {
      throw new Error("instance not found");
    }
    return instance;
  }

  async create(options: {
    id: string;
    params: JobMessage;
  }): Promise<WorkflowInstanceHandle> {
    const instance: WorkflowInstanceHandle = {
      id: options.id,
      status: async () => ({ status: "queued", output: null }),
    };
    this.instances.set(options.id, instance);
    return instance;
  }
}

function createTestContext() {
  const jobQueue = new FakeQueue<JobMessage>();
  const auditQueue = new FakeQueue<AuditMessage>();
  const workflow = new FakeWorkflow();
  const env: ApiBindings = {
    JOB_QUEUE: jobQueue,
    AUDIT_QUEUE: auditQueue,
    JOB_WORKFLOW: workflow,
  };
  const app = createApp({
    createJobId: () => "job-123",
    now: () => "2026-07-26T00:00:00.000Z",
  });

  return { app, env, jobQueue, auditQueue, workflow };
}

describe("GET /health", () => {
  it("reports the Worker service as healthy", async () => {
    const { app, env } = createTestContext();

    const response = await app.request("/health", {}, env);

    expect(response.status).toBe(200);
    expect(await response.json()).toEqual({
      status: "ok",
      service: "workers-sandbox-api",
    });
  });
});

describe("POST /jobs", () => {
  it("queues a validated job and returns a polling URL", async () => {
    const { app, env, jobQueue, auditQueue } = createTestContext();

    const response = await app.request(
      "/jobs",
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          target: { id: "customer-42", values: [10, 20, 30] },
          operation: "average",
        }),
      },
      env,
    );

    expect(response.status).toBe(202);
    expect(await response.json()).toEqual({
      jobId: "job-123",
      statusUrl: "/jobs/job-123",
    });
    expect(jobQueue.sent).toEqual([
      {
        jobId: "job-123",
        submittedAt: "2026-07-26T00:00:00.000Z",
        target: { id: "customer-42", values: [10, 20, 30] },
        operation: "average",
        failStepBOnce: false,
      },
    ]);
    expect(auditQueue.sent).toEqual([]);
  });

  it("explicitly fans out an audit event when requested", async () => {
    const { app, env, auditQueue } = createTestContext();

    const response = await app.request(
      "/jobs",
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          target: { id: "customer-42", values: [10] },
          operation: "sum",
          fanout: true,
          failStepBOnce: true,
        }),
      },
      env,
    );

    expect(response.status).toBe(202);
    expect(auditQueue.sent).toEqual([
      {
        eventType: "job.submitted",
        jobId: "job-123",
        submittedAt: "2026-07-26T00:00:00.000Z",
        targetId: "customer-42",
      },
    ]);
  });

  it("returns a stable validation error without queueing", async () => {
    const { app, env, jobQueue } = createTestContext();

    const response = await app.request(
      "/jobs",
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          target: { id: "", values: [] },
          operation: "sum",
        }),
      },
      env,
    );

    expect(response.status).toBe(400);
    expect(await response.json()).toEqual({
      error: {
        code: "INVALID_JOB",
        message: "target.id must be a non-empty string",
      },
    });
    expect(jobQueue.sent).toEqual([]);
  });

  it("returns INVALID_JSON for a malformed body", async () => {
    const { app, env } = createTestContext();

    const response = await app.request(
      "/jobs",
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: "{",
      },
      env,
    );

    expect(response.status).toBe(400);
    expect(await response.json()).toEqual({
      error: {
        code: "INVALID_JSON",
        message: "request body must be valid JSON",
      },
    });
  });
});

describe("GET /jobs/:jobId", () => {
  it("returns the Workflow instance status", async () => {
    const { app, env, workflow } = createTestContext();
    workflow.instances.set("job-finished", {
      id: "job-finished",
      status: async () => ({
        status: "complete",
        output: {
          targetId: "customer-42",
          operation: "sum",
          result: 60,
        },
      }),
    });

    const response = await app.request("/jobs/job-finished", {}, env);

    expect(response.status).toBe(200);
    expect(await response.json()).toEqual({
      jobId: "job-finished",
      workflow: {
        status: "complete",
        output: {
          targetId: "customer-42",
          operation: "sum",
          result: 60,
        },
      },
    });
  });

  it("returns a polling-aware response before the Workflow exists", async () => {
    const { app, env } = createTestContext();

    const response = await app.request("/jobs/not-started", {}, env);

    expect(response.status).toBe(404);
    expect(await response.json()).toEqual({
      error: {
        code: "JOB_NOT_STARTED",
        message:
          "Workflow instance not found; the Queue consumer may not have started it yet",
      },
    });
  });
});
