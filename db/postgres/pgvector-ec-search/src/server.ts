import "dotenv/config";

import { serve } from "@hono/node-server";
import { desc } from "drizzle-orm";
import { Hono } from "hono";
import { z } from "zod";

import { db } from "./db.js";
import { searchProducts } from "./search.js";
import { products } from "./schema.js";

const searchRequestSchema = z.object({
  query: z.string().min(1),
  category: z.string().min(1).optional(),
  minPriceCents: z.number().int().nonnegative().optional(),
  maxPriceCents: z.number().int().nonnegative().optional(),
  targetPriceCents: z.number().int().positive().optional(),
  limit: z.number().int().min(1).max(20).optional(),
  weights: z
    .object({
      semantic: z.number().nonnegative().optional(),
      popularity: z.number().nonnegative().optional(),
      rating: z.number().nonnegative().optional(),
      priceFit: z.number().nonnegative().optional(),
    })
    .optional(),
});

const app = new Hono();

app.get("/health", (context) => context.json({ ok: true }));

app.get("/products", async (context) => {
  const rows = await db.select().from(products).orderBy(desc(products.popularityScore));
  return context.json({ products: rows });
});

app.post("/search", async (context) => {
  const body = await context.req.json().catch(() => undefined);
  const parsed = searchRequestSchema.safeParse(body);

  if (!parsed.success) {
    return context.json({ error: "Invalid search request", issues: parsed.error.issues }, 400);
  }

  const results = await searchProducts(parsed.data);
  return context.json({ results });
});

const port = Number(process.env.PORT ?? 8787);

serve({ fetch: app.fetch, port }, (info) => {
  console.log(`PG Vector EC search API listening on http://localhost:${info.port}`);
});

export { app };
