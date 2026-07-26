import { describe, expect, it } from "vitest";
import {
  consumeQueue,
  processAuditMessage,
  processJobMessage,
  type ConsumerBindings,
  type QueueBatch,
  type QueueMessage,
  type WorkflowController,
  type WorkflowHandle,
} from "../src/consumer";
import type { AuditMessage, JobMessage } from "../src/messages";

class FakeMessage<T> implements QueueMessage<T> {
  readonly id = "message-1";
  readonly timestamp = new Date("2026-07-26T00:00:00.000Z");
  readonly attempts = 1;
  ackCalls = 0;
  retryCalls = 0;

  constructor(readonly body: T) {}

  ack(): void {
    this.ackCalls += 1;
  }

  retry(): void {
    this.retryCalls += 1;
  }
}

class FakeWorkflow implements WorkflowController<JobMessage> {
  readonly instances = new Map<string, WorkflowHandle>();
  readonly createCalls: Array<{ id: string; params: JobMessage }> = [];
  createFailure: Error | undefined;
  addInstanceBeforeCreateFailure = false;

  async get(id: string): Promise<WorkflowHandle> {
    const instance = this.instances.get(id);
    if (!instance) {
      throw new Error("instance not found");
    }
    return instance;
  }

  async create(options: {
    id: string;
    params: JobMessage;
  }): Promise<WorkflowHandle> {
    this.createCalls.push(options);
    const instance: WorkflowHandle = { id: options.id };

    if (this.createFailure) {
      if (this.addInstanceBeforeCreateFailure) {
        this.instances.set(options.id, instance);
      }
      throw this.createFailure;
    }

    this.instances.set(options.id, instance);
    return instance;
  }
}

const job: JobMessage = {
  jobId: "job-123",
  submittedAt: "2026-07-26T00:00:00.000Z",
  target: { id: "customer-42", values: [10, 20, 30] },
  operation: "average",
  failStepBOnce: false,
};

const audit: AuditMessage = {
  eventType: "job.submitted",
  jobId: "job-123",
  submittedAt: "2026-07-26T00:00:00.000Z",
  targetId: "customer-42",
};

function batch<T>(queue: string, messages: QueueMessage<T>[]): QueueBatch<T> {
  return { queue, messages };
}

describe("processJobMessage", () => {
  it("starts a Workflow with the job ID and acknowledges the message", async () => {
    const message = new FakeMessage(job);
    const workflow = new FakeWorkflow();

    await processJobMessage(message, workflow);

    expect(workflow.createCalls).toEqual([{ id: "job-123", params: job }]);
    expect(message.ackCalls).toBe(1);
    expect(message.retryCalls).toBe(0);
  });

  it("acknowledges an at-least-once duplicate without starting another Workflow", async () => {
    const message = new FakeMessage(job);
    const workflow = new FakeWorkflow();
    workflow.instances.set("job-123", { id: "job-123" });

    await processJobMessage(message, workflow);

    expect(workflow.createCalls).toEqual([]);
    expect(message.ackCalls).toBe(1);
  });

  it("acknowledges a concurrent duplicate detected after create fails", async () => {
    const message = new FakeMessage(job);
    const workflow = new FakeWorkflow();
    workflow.createFailure = new Error("instance already exists");
    workflow.addInstanceBeforeCreateFailure = true;

    await processJobMessage(message, workflow);

    expect(workflow.createCalls).toHaveLength(1);
    expect(message.ackCalls).toBe(1);
    expect(message.retryCalls).toBe(0);
  });

  it("retries when create fails and no Workflow instance exists", async () => {
    const message = new FakeMessage(job);
    const workflow = new FakeWorkflow();
    workflow.createFailure = new Error("temporary Workflow outage");

    await processJobMessage(message, workflow);

    expect(message.ackCalls).toBe(0);
    expect(message.retryCalls).toBe(1);
  });
});

describe("processAuditMessage", () => {
  it("writes a structured log and acknowledges the message", async () => {
    const message = new FakeMessage(audit);
    const lines: string[] = [];

    await processAuditMessage(message, {
      log(line) {
        lines.push(String(line));
      },
    });

    expect(lines.map((line) => JSON.parse(line))).toEqual([
      {
        event: "audit.job-submitted",
        jobId: "job-123",
        messageId: "message-1",
        submittedAt: "2026-07-26T00:00:00.000Z",
        targetId: "customer-42",
      },
    ]);
    expect(message.ackCalls).toBe(1);
  });
});

describe("consumeQueue", () => {
  it("routes messages from each configured queue", async () => {
    const jobMessage = new FakeMessage<unknown>(job);
    const auditMessage = new FakeMessage<unknown>(audit);
    const workflow = new FakeWorkflow();
    const logs: string[] = [];
    const env: ConsumerBindings = { JOB_WORKFLOW: workflow };

    await consumeQueue(batch("workers-sandbox-jobs", [jobMessage]), env);
    await consumeQueue(
      batch("workers-sandbox-audit", [auditMessage]),
      env,
      {
        log(line) {
          logs.push(String(line));
        },
        error() {},
      },
    );

    expect(jobMessage.ackCalls).toBe(1);
    expect(auditMessage.ackCalls).toBe(1);
    expect(logs).toHaveLength(1);
  });

  it("retries a malformed message instead of acknowledging it", async () => {
    const message = new FakeMessage<unknown>({ jobId: "missing-fields" });
    const env: ConsumerBindings = { JOB_WORKFLOW: new FakeWorkflow() };

    await consumeQueue(batch("workers-sandbox-jobs", [message]), env, {
      log() {},
      error() {},
    });

    expect(message.ackCalls).toBe(0);
    expect(message.retryCalls).toBe(1);
  });

  it("throws for an unconfigured queue", async () => {
    const env: ConsumerBindings = { JOB_WORKFLOW: new FakeWorkflow() };

    await expect(
      consumeQueue(batch("unknown-queue", []), env),
    ).rejects.toThrow("No consumer is configured for queue: unknown-queue");
  });
});
