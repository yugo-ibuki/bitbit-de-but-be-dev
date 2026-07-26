import {
  WorkflowEntrypoint,
  type WorkflowEvent,
  type WorkflowStep,
} from "cloudflare:workers";
import { calculateResult, transformInput } from "./job";
import type { JobMessage } from "./messages";

export class JobWorkflow extends WorkflowEntrypoint<Env, JobMessage> {
  async run(
    event: Readonly<WorkflowEvent<JobMessage>>,
    step: WorkflowStep,
  ) {
    const transformed = await step.do("transform-input", async (context) => {
      console.log(
        JSON.stringify({
          event: "workflow.step",
          jobId: event.payload.jobId,
          step: context.step.name,
          attempt: context.attempt,
        }),
      );

      return transformInput(event.payload.target);
    });

    return step.do(
      "calculate-result",
      {
        retries: {
          limit: 3,
          delay: "1 second",
          backoff: "constant",
        },
        timeout: "30 seconds",
      },
      async (context) => {
        console.log(
          JSON.stringify({
            event: "workflow.step",
            jobId: event.payload.jobId,
            step: context.step.name,
            attempt: context.attempt,
          }),
        );

        if (event.payload.failStepBOnce && context.attempt === 1) {
          throw new Error("Intentional first-attempt failure");
        }

        return {
          ...calculateResult(transformed, event.payload.operation),
          stepBAttempt: context.attempt,
        };
      },
    );
  }
}
