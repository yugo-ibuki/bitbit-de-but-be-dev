import { and, asc, cosineDistance, eq, gte, lte } from "drizzle-orm";

import { db } from "./db.js";
import { createQueryEmbedding } from "./embedding.js";
import { DEFAULT_WEIGHTS, rankSearchResult, type SearchWeights } from "./ranking.js";
import { products, type Product } from "./schema.js";

export type ProductSearchInput = {
  query: string;
  category?: string;
  minPriceCents?: number;
  maxPriceCents?: number;
  targetPriceCents?: number;
  weights?: Partial<SearchWeights>;
  limit?: number;
};

export type ProductSearchResult = {
  product: Product;
  cosineDistance: number;
  semanticSimilarity: number;
  weightedScore: number;
};

export async function searchProducts(input: ProductSearchInput): Promise<ProductSearchResult[]> {
  const limit = clampInteger(input.limit ?? 5, 1, 20);
  const candidateLimit = Math.max(limit * 4, 12);
  const queryEmbedding = createQueryEmbedding(input.query);
  const distance = cosineDistance(products.embedding, queryEmbedding);
  const conditions = [
    input.category ? eq(products.category, input.category) : undefined,
    input.minPriceCents ? gte(products.priceCents, input.minPriceCents) : undefined,
    input.maxPriceCents ? lte(products.priceCents, input.maxPriceCents) : undefined,
  ].filter((condition) => condition !== undefined);

  const rows = await db
    .select({
      product: products,
      cosineDistance: distance,
    })
    .from(products)
    .where(conditions.length > 0 ? and(...conditions) : undefined)
    .orderBy(asc(distance))
    .limit(candidateLimit);

  const weights = {
    ...DEFAULT_WEIGHTS,
    ...input.weights,
  };

  return rows
    .map((row) => {
      const cosineDistanceValue = Number(row.cosineDistance);
      const weightedScore = rankSearchResult({
        cosineDistance: cosineDistanceValue,
        popularityScore: row.product.popularityScore,
        rating: row.product.rating,
        priceCents: row.product.priceCents,
        targetPriceCents: input.targetPriceCents,
        weights,
      });

      return {
        product: row.product,
        cosineDistance: cosineDistanceValue,
        semanticSimilarity: Number((1 - cosineDistanceValue).toFixed(6)),
        weightedScore,
      };
    })
    .sort((left, right) => right.weightedScore - left.weightedScore)
    .slice(0, limit);
}

function clampInteger(value: number, min: number, max: number): number {
  return Math.min(Math.max(Math.trunc(value), min), max);
}
