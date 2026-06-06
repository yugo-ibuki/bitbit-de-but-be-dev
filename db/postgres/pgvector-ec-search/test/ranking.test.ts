import { describe, expect, test } from "vitest";

import { rankSearchResult } from "../src/ranking.js";

describe("weighted ranking", () => {
  test("combines vector similarity with popularity, rating, and price fit", () => {
    const score = rankSearchResult({
      cosineDistance: 0.2,
      popularityScore: 80,
      rating: 4.6,
      priceCents: 12_000,
      targetPriceCents: 10_000,
      weights: {
        semantic: 0.7,
        popularity: 0.15,
        rating: 0.1,
        priceFit: 0.05,
      },
    });

    expect(score).toBeGreaterThan(0);
    expect(score).toBeLessThanOrEqual(1);
  });

  test("favors cheaper similarly relevant products when price weight is high", () => {
    const weights = {
      semantic: 0.4,
      popularity: 0.05,
      rating: 0.05,
      priceFit: 0.5,
    };
    const cheaper = rankSearchResult({
      cosineDistance: 0.18,
      popularityScore: 60,
      rating: 4.2,
      priceCents: 8_000,
      targetPriceCents: 10_000,
      weights,
    });
    const expensive = rankSearchResult({
      cosineDistance: 0.16,
      popularityScore: 60,
      rating: 4.2,
      priceCents: 60_000,
      targetPriceCents: 10_000,
      weights,
    });

    expect(cheaper).toBeGreaterThan(expensive);
  });
});
