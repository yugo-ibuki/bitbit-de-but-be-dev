export type Operation = "sum" | "average" | "max";

export type JobTarget = {
  id: string;
  values: number[];
};

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

export function transformInput(target: JobTarget): TransformedInput {
  const values = [...target.values];

  return {
    targetId: target.id,
    values,
    count: values.length,
    sum: values.reduce((total, value) => total + value, 0),
  };
}

export function calculateResult(
  input: TransformedInput,
  operation: Operation,
): JobResult {
  let result: number;

  switch (operation) {
    case "sum":
      result = input.sum;
      break;
    case "average":
      result = input.sum / input.count;
      break;
    case "max":
      result = Math.max(...input.values);
      break;
  }

  return {
    targetId: input.targetId,
    operation,
    result,
  };
}
