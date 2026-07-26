import type { JobTarget, Operation } from "./job";

export type JobRequest = {
  target: JobTarget;
  operation: Operation;
  fanout: boolean;
  failStepBOnce: boolean;
};

export type JobMessage = {
  jobId: string;
  submittedAt: string;
  target: JobTarget;
  operation: Operation;
  failStepBOnce: boolean;
};

export type AuditMessage = {
  eventType: "job.submitted";
  jobId: string;
  submittedAt: string;
  targetId: string;
};

export type JobValidationError = {
  code: "INVALID_JOB";
  message: string;
};

export type JobRequestParseResult =
  | { success: true; data: JobRequest }
  | { success: false; error: JobValidationError };

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function invalid(message: string): JobRequestParseResult {
  return {
    success: false,
    error: {
      code: "INVALID_JOB",
      message,
    },
  };
}

function isOperation(value: unknown): value is Operation {
  return value === "sum" || value === "average" || value === "max";
}

export function parseJobRequest(value: unknown): JobRequestParseResult {
  if (!isRecord(value) || !isRecord(value.target)) {
    return invalid("target must be an object");
  }

  const rawId = value.target.id;
  if (typeof rawId !== "string" || rawId.trim().length === 0) {
    return invalid("target.id must be a non-empty string");
  }

  const id = rawId.trim();
  if (id.length > 100) {
    return invalid("target.id must be at most 100 characters");
  }

  const values = value.target.values;
  if (!Array.isArray(values) || values.length < 1 || values.length > 100) {
    return invalid("target.values must contain between 1 and 100 numbers");
  }

  if (!values.every((entry) => typeof entry === "number" && Number.isFinite(entry))) {
    return invalid("target.values must contain only finite numbers");
  }

  if (!isOperation(value.operation)) {
    return invalid("operation must be one of: sum, average, max");
  }

  if (value.fanout !== undefined && typeof value.fanout !== "boolean") {
    return invalid("fanout must be a boolean");
  }

  if (
    value.failStepBOnce !== undefined &&
    typeof value.failStepBOnce !== "boolean"
  ) {
    return invalid("failStepBOnce must be a boolean");
  }

  return {
    success: true,
    data: {
      target: {
        id,
        values: [...values],
      },
      operation: value.operation,
      fanout: value.fanout ?? false,
      failStepBOnce: value.failStepBOnce ?? false,
    },
  };
}
