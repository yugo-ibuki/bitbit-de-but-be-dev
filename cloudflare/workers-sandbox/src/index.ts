import { createApp } from "./api";
import { consumeQueue } from "./consumer";

export { JobWorkflow } from "./workflow";

const app = createApp();

export default {
  fetch(
    request: Request,
    env: Env,
    ctx: ExecutionContext,
  ): Response | Promise<Response> {
    return app.fetch(request, env, ctx);
  },
  queue(batch: MessageBatch<unknown>, env: Env): Promise<void> {
    return consumeQueue(batch, env);
  },
} satisfies ExportedHandler<Env>;
