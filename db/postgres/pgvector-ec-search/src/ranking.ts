export type SearchWeights = {
  semantic: number;
  popularity: number;
  rating: number;
  priceFit: number;
};

export type RankSearchResultInput = {
  cosineDistance: number;
  popularityScore: number;
  rating: number;
  priceCents: number;
  targetPriceCents?: number;
  weights: SearchWeights;
};

export const DEFAULT_WEIGHTS: SearchWeights = {
  semantic: 0.72,
  popularity: 0.12,
  rating: 0.1,
  priceFit: 0.06,
};

export function rankSearchResult(input: RankSearchResultInput): number {
  const normalizedWeights = normalizeWeights(input.weights);
  const semanticScore = clamp(1 - input.cosineDistance, 0, 1);
  const popularityScore = clamp(input.popularityScore / 100, 0, 1);
  const ratingScore = clamp(input.rating / 5, 0, 1);
  const priceFitScore = calculatePriceFit(input.priceCents, input.targetPriceCents);

  const score =
    semanticScore * normalizedWeights.semantic +
    popularityScore * normalizedWeights.popularity +
    ratingScore * normalizedWeights.rating +
    priceFitScore * normalizedWeights.priceFit;

  return Number(clamp(score, 0, 1).toFixed(6));
}

export function normalizeWeights(weights: SearchWeights): SearchWeights {
  const sanitized = {
    semantic: Math.max(weights.semantic, 0),
    popularity: Math.max(weights.popularity, 0),
    rating: Math.max(weights.rating, 0),
    priceFit: Math.max(weights.priceFit, 0),
  };
  const total = sanitized.semantic + sanitized.popularity + sanitized.rating + sanitized.priceFit;

  if (total === 0) {
    return DEFAULT_WEIGHTS;
  }

  return {
    semantic: sanitized.semantic / total,
    popularity: sanitized.popularity / total,
    rating: sanitized.rating / total,
    priceFit: sanitized.priceFit / total,
  };
}

function calculatePriceFit(priceCents: number, targetPriceCents?: number): number {
  if (!targetPriceCents || targetPriceCents <= 0) {
    return 1;
  }

  const differenceRatio = Math.abs(priceCents - targetPriceCents) / targetPriceCents;
  return clamp(1 - differenceRatio, 0, 1);
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(Math.max(value, min), max);
}
