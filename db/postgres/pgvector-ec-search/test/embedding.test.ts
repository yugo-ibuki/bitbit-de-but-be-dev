import { describe, expect, test } from "vitest";

import { EMBEDDING_DIMENSIONS, createProductEmbedding, createQueryEmbedding } from "../src/embedding.js";

describe("local embedding", () => {
  test("creates deterministic vectors with the configured pgvector dimensions", () => {
    const product = {
      name: "Trail running shoes",
      category: "shoes",
      description: "Lightweight waterproof shoes for trail running",
      tags: ["outdoor", "running", "waterproof"],
    };

    const first = createProductEmbedding(product);
    const second = createProductEmbedding(product);

    expect(first).toHaveLength(EMBEDDING_DIMENSIONS);
    expect(second).toEqual(first);
  });

  test("places related product and query text closer than unrelated text", () => {
    const outdoorShoes = createProductEmbedding({
      name: "Trail running shoes",
      category: "shoes",
      description: "Lightweight waterproof shoes for mountain trails",
      tags: ["outdoor", "running", "waterproof"],
    });
    const kitchenMixer = createProductEmbedding({
      name: "Kitchen stand mixer",
      category: "kitchen",
      description: "Large mixer for baking bread and cakes",
      tags: ["cooking", "baking", "home"],
    });
    const query = createQueryEmbedding("waterproof running shoes for outdoor trails");

    const distanceToShoes = cosineDistance(outdoorShoes, query);
    const distanceToMixer = cosineDistance(kitchenMixer, query);

    expect(distanceToShoes).toBeLessThan(distanceToMixer);
  });
});

function cosineDistance(left: number[], right: number[]): number {
  const dot = left.reduce((sum, value, index) => sum + value * right[index]!, 0);
  const leftNorm = Math.sqrt(left.reduce((sum, value) => sum + value * value, 0));
  const rightNorm = Math.sqrt(right.reduce((sum, value) => sum + value * value, 0));

  return 1 - dot / (leftNorm * rightNorm);
}
