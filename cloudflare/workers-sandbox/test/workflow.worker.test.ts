import { env } from "cloudflare:workers";
import { introspectWorkflowInstance } from "cloudflare:test";
import { describe, expect, it } from "vitest";
import type { JobMessage } from "../src/messages";

function job(overrides: Partial<JobMessage> = {}): JobMessage {
  return {
    jobId: crypto.randomUUID(),
    submittedAt: "2026-07-26T00:00:00.000Z",
    target: { id: "customer-42", values: [10, 20, 30] },
    operation: "average",
    failStepBOnce: false,
    ...overrides,
  };
}

describe("JobWorkflow", () => {
  it("persists Step A output and passes it to Step B", async () => {
    const payload = job();
    const introspector = await introspectWorkflowInstance(
      env.JOB_WORKFLOW,
      payload.jobId,
    );

    try {
      await env.JOB_WORKFLOW.create({
        id: payload.jobId,
        params: payload,
      });

      await expect(
        introspector.waitForStepResult({ name: "transform-input" }),
      ).resolves.toEqual({
        targetId: "customer-42",
        values: [10, 20, 30],
        count: 3,
        sum: 60,
      });
      await expect(introspector.waitForStatus("complete")).resolves.toBeUndefined();
      await expect(introspector.getOutput()).resolves.toEqual({
        targetId: "customer-42",
        operation: "average",
        result: 20,
        stepBAttempt: 1,
      });
    } finally {
      await introspector.dispose();
    }
  });

  it("retries only Step B after an intentional first-attempt failure", async () => {
    const payload = job({
      jobId: crypto.randomUUID(),
      failStepBOnce: true,
    });
    const introspector = await introspectWorkflowInstance(
      env.JOB_WORKFLOW,
      payload.jobId,
    );

    try {
      await introspector.modify(async (modifier) => {
        await modifier.disableRetryDelays([{ name: "calculate-result" }]);
      });

      await env.JOB_WORKFLOW.create({
        id: payload.jobId,
        params: payload,
      });

      await expect(introspector.waitForStatus("complete")).resolves.toBeUndefined();
      await expect(introspector.getOutput()).resolves.toMatchObject({
        targetId: "customer-42",
        result: 20,
        stepBAttempt: 2,
      });
      await expect(
        introspector.waitForStepResult({ name: "transform-input" }),
      ).resolves.toMatchObject({
        count: 3,
        sum: 60,
      });
    } finally {
      await introspector.dispose();
    }
  });
});
