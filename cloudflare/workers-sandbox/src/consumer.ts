import {
  isAuditMessage,
  isJobMessage,
  type AuditMessage,
  type JobMessage,
} from "./messages";

export type QueueMessage<T> = {
  readonly id: string;
  readonly timestamp: Date;
  readonly body: T;
  readonly attempts: number;
  ack(): void;
  retry(options?: { delaySeconds?: number }): void;
};

export type QueueBatch<T> = {
  readonly queue: string;
  readonly messages: readonly QueueMessage<T>[];
};

export type WorkflowHandle = {
  id: string;
};

export type WorkflowController<T> = {
  get(id: string): Promise<WorkflowHandle>;
  create(options: { id: string; params: T }): Promise<WorkflowHandle>;
};

export type ConsumerBindings = {
  JOB_WORKFLOW: WorkflowController<JobMessage>;
};

type Logger = Pick<Console, "log" | "error">;

async function findWorkflow(
  workflow: WorkflowController<JobMessage>,
  jobId: string,
): Promise<WorkflowHandle | undefined> {
  try {
    return await workflow.get(jobId);
  } catch {
    return undefined;
  }
}

export async function processJobMessage(
  message: QueueMessage<unknown>,
  workflow: WorkflowController<JobMessage>,
  logger: Logger = console,
): Promise<void> {
  if (!isJobMessage(message.body)) {
    logger.error(
      JSON.stringify({
        event: "queue.job.invalid",
        messageId: message.id,
        attempts: message.attempts,
      }),
    );
    message.retry();
    return;
  }

  const job = message.body;
  const existing = await findWorkflow(workflow, job.jobId);
  if (existing) {
    logger.log(
      JSON.stringify({
        event: "queue.job.duplicate",
        jobId: job.jobId,
        messageId: message.id,
      }),
    );
    message.ack();
    return;
  }

  try {
    await workflow.create({
      id: job.jobId,
      params: job,
    });
    logger.log(
      JSON.stringify({
        event: "queue.job.workflow-started",
        jobId: job.jobId,
        messageId: message.id,
      }),
    );
    message.ack();
  } catch (error) {
    const racedInstance = await findWorkflow(workflow, job.jobId);
    if (racedInstance) {
      logger.log(
        JSON.stringify({
          event: "queue.job.concurrent-duplicate",
          jobId: job.jobId,
          messageId: message.id,
        }),
      );
      message.ack();
      return;
    }

    logger.error(
      JSON.stringify({
        event: "queue.job.workflow-start-failed",
        jobId: job.jobId,
        messageId: message.id,
        attempts: message.attempts,
        error: error instanceof Error ? error.message : String(error),
      }),
    );
    message.retry();
  }
}

export async function processAuditMessage(
  message: QueueMessage<unknown>,
  logger: Pick<Console, "log"> = console,
): Promise<void> {
  if (!isAuditMessage(message.body)) {
    message.retry();
    return;
  }

  const audit: AuditMessage = message.body;
  logger.log(
    JSON.stringify({
      event: "audit.job-submitted",
      jobId: audit.jobId,
      messageId: message.id,
      submittedAt: audit.submittedAt,
      targetId: audit.targetId,
    }),
  );
  message.ack();
}

export async function consumeQueue(
  batch: QueueBatch<unknown>,
  env: ConsumerBindings,
  logger: Logger = console,
): Promise<void> {
  switch (batch.queue) {
    case "workers-sandbox-jobs":
      await Promise.all(
        batch.messages.map((message) =>
          processJobMessage(message, env.JOB_WORKFLOW, logger),
        ),
      );
      return;
    case "workers-sandbox-audit":
      await Promise.all(
        batch.messages.map((message) => processAuditMessage(message, logger)),
      );
      return;
    default:
      throw new Error(`No consumer is configured for queue: ${batch.queue}`);
  }
}
