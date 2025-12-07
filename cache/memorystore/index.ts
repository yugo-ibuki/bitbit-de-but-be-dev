import Fastify from "fastify";
import { createClient, type RedisClientType } from "redis";

const HOST = process.env.REDIS_HOST ?? "localhost";
const PORT = parseInt(process.env.REDIS_PORT ?? "6379", 10);
const CACHE_TTL = 10; // seconds

let redis: RedisClientType;

const fastify = Fastify({ logger: true });

// 重い処理をシミュレート
const heavyComputation = async (id: string): Promise<{ id: string; data: string; computedAt: string }> => {
  await new Promise((resolve) => setTimeout(resolve, 2000)); // 2秒待機
  return {
    id,
    data: `Result for ${id}`,
    computedAt: new Date().toISOString(),
  };
};

// GET /data/:id - キャッシュありでデータ取得
fastify.get<{ Params: { id: string } }>("/data/:id", async (request, reply) => {
  const { id } = request.params;
  const cacheKey = `data:${id}`;
  const start = Date.now();

  // キャッシュ確認
  const cached = await redis.get(cacheKey);
  if (cached) {
    const elapsed = Date.now() - start;
    return {
      source: "cache",
      elapsed_ms: elapsed,
      data: JSON.parse(cached),
    };
  }

  // キャッシュミス -> 重い処理を実行
  const result = await heavyComputation(id);
  await redis.setEx(cacheKey, CACHE_TTL, JSON.stringify(result));

  const elapsed = Date.now() - start;
  return {
    source: "computed",
    elapsed_ms: elapsed,
    data: result,
  };
});

// DELETE /data/:id - キャッシュ削除
fastify.delete<{ Params: { id: string } }>("/data/:id", async (request) => {
  const { id } = request.params;
  const deleted = await redis.del(`data:${id}`);
  return { deleted: deleted > 0 };
});

// GET /health
fastify.get("/health", async () => {
  const pong = await redis.ping();
  return { status: "ok", redis: pong };
});

const start = async () => {
  redis = createClient({ url: `redis://${HOST}:${PORT}` });
  redis.on("error", (err) => console.error("Redis error:", err));
  await redis.connect();

  await fastify.listen({ port: 8080, host: "0.0.0.0" });
};

start();
