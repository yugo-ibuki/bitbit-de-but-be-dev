import { describe, expect, it } from "vitest";
import {
  calculateResult,
  transformInput,
  type TransformedInput,
} from "../src/job";
import { parseJobRequest } from "../src/messages";

describe("transformInput", () => {
  it("adds the derived count and sum without changing the values", () => {
    expect(
      transformInput({ id: "customer-42", values: [10, 20, 30] }),
    ).toEqual({
      targetId: "customer-42",
      values: [10, 20, 30],
      count: 3,
      sum: 60,
    });
  });
});

describe("calculateResult", () => {
  const transformed: TransformedInput = {
    targetId: "customer-42",
    values: [10, 20, 30],
    count: 3,
    sum: 60,
  };

  it.each([
    ["sum", 60],
    ["average", 20],
    ["max", 30],
  ] as const)("calculates the %s operation", (operation, result) => {
    expect(calculateResult(transformed, operation)).toEqual({
      targetId: "customer-42",
      operation,
      result,
    });
  });
});

describe("parseJobRequest", () => {
  it("normalizes a valid request and defaults optional flags", () => {
    expect(
      parseJobRequest({
        target: { id: "  customer-42  ", values: [10, 20, 30] },
        operation: "average",
      }),
    ).toEqual({
      success: true,
      data: {
        target: { id: "customer-42", values: [10, 20, 30] },
        operation: "average",
        fanout: false,
        failStepBOnce: false,
      },
    });
  });

  it("preserves explicit optional flags", () => {
    const result = parseJobRequest({
      target: { id: "customer-42", values: [1] },
      operation: "sum",
      fanout: true,
      failStepBOnce: true,
    });

    expect(result.success).toBe(true);
    if (result.success) {
      expect(result.data.fanout).toBe(true);
      expect(result.data.failStepBOnce).toBe(true);
    }
  });

  it.each([
    [
      { target: { id: "", values: [1] }, operation: "sum" },
      "target.id must be a non-empty string",
    ],
    [
      {
        target: { id: "x".repeat(101), values: [1] },
        operation: "sum",
      },
      "target.id must be at most 100 characters",
    ],
    [
      { target: { id: "x", values: [] }, operation: "sum" },
      "target.values must contain between 1 and 100 numbers",
    ],
    [
      {
        target: { id: "x", values: Array.from({ length: 101 }, () => 1) },
        operation: "sum",
      },
      "target.values must contain between 1 and 100 numbers",
    ],
    [
      { target: { id: "x", values: [Number.POSITIVE_INFINITY] }, operation: "sum" },
      "target.values must contain only finite numbers",
    ],
    [
      { target: { id: "x", values: [1] }, operation: "median" },
      "operation must be one of: sum, average, max",
    ],
    [
      {
        target: { id: "x", values: [1] },
        operation: "sum",
        fanout: "yes",
      },
      "fanout must be a boolean",
    ],
    [
      {
        target: { id: "x", values: [1] },
        operation: "sum",
        failStepBOnce: 1,
      },
      "failStepBOnce must be a boolean",
    ],
  ])("rejects invalid input: %s", (input, message) => {
    expect(parseJobRequest(input)).toEqual({
      success: false,
      error: {
        code: "INVALID_JOB",
        message,
      },
    });
  });
});
