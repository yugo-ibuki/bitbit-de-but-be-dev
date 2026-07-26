import { Hono } from "hono";
import type { AuditMessage, JobMessage } from "./messages";
import { parseJobRequest } from "./messages";

export type QueueSender<T> = {
  send(body: T): Promise<unknown>;
};

export type WorkflowInstanceHandle = {
  id: string;
  status(): Promise<unknown>;
};

export type WorkflowReader = {
  get(id: string): Promise<WorkflowInstanceHandle>;
};

export type ApiBindings = {
  JOB_QUEUE: QueueSender<JobMessage>;
  AUDIT_QUEUE: QueueSender<AuditMessage>;
  JOB_WORKFLOW: WorkflowReader;
};

type AppOptions = {
  createJobId?: () => string;
  now?: () => string;
};

export function createApp(options: AppOptions = {}) {
  const createJobId = options.createJobId ?? (() => crypto.randomUUID());
  const now = options.now ?? (() => new Date().toISOString());
  const app = new Hono<{ Bindings: ApiBindings }>();

  app.get("/health", (c) =>
    c.json({
      status: "ok",
      service: "workers-sandbox-api",
    }),
  );

  app.post("/jobs", async (c) => {
    let body: unknown;
    try {
      body = await c.req.json<unknown>();
    } catch {
      return c.json(
        {
          error: {
            code: "INVALID_JSON",
            message: "request body must be valid JSON",
          },
        },
        400,
      );
    }

    const parsed = parseJobRequest(body);
    if (!parsed.success) {
      return c.json({ error: parsed.error }, 400);
    }

    const jobId = createJobId();
    const submittedAt = now();
    const message: JobMessage = {
      jobId,
      submittedAt,
      target: parsed.data.target,
      operation: parsed.data.operation,
      failStepBOnce: parsed.data.failStepBOnce,
    };

    await c.env.JOB_QUEUE.send(message);

    if (parsed.data.fanout) {
      const auditMessage: AuditMessage = {
        eventType: "job.submitted",
        jobId,
        submittedAt,
        targetId: parsed.data.target.id,
      };
      await c.env.AUDIT_QUEUE.send(auditMessage);
    }

    return c.json(
      {
        jobId,
        statusUrl: `/jobs/${jobId}`,
      },
      202,
    );
  });

  app.get("/jobs/:jobId", async (c) => {
    const jobId = c.req.param("jobId");

    try {
      const instance = await c.env.JOB_WORKFLOW.get(jobId);
      return c.json({
        jobId,
        workflow: await instance.status(),
      });
    } catch {
      return c.json(
        {
          error: {
            code: "JOB_NOT_STARTED",
            message:
              "Workflow instance not found; the Queue consumer may not have started it yet",
          },
        },
        404,
      );
    }
  });

  return app;
}
