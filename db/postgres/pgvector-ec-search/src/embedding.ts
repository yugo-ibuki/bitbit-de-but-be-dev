export const EMBEDDING_DIMENSIONS = 12;

type EmbeddableProduct = {
  name: string;
  category: string;
  description: string;
  tags: string[];
};

const FEATURE_KEYWORDS: string[][] = [
  ["outdoor", "trail", "mountain", "camp", "camping", "hike", "hiking"],
  ["running", "run", "runner", "jogging", "training"],
  ["waterproof", "rain", "wet", "water", "breathable"],
  ["shoe", "shoes", "sneaker", "sneakers", "footwear", "boots"],
  ["kitchen", "cooking", "cook", "cookware", "pan", "pot"],
  ["baking", "bread", "cake", "mixer", "oven"],
  ["home", "room", "living", "interior", "desk"],
  ["electronics", "tech", "gadget", "wireless", "battery"],
  ["coffee", "drink", "beverage", "mug", "espresso"],
  ["fitness", "sports", "yoga", "workout", "gym"],
  ["premium", "pro", "high-end", "luxury", "durable"],
  ["compact", "lightweight", "portable", "small", "mini"],
];

export function createProductEmbedding(product: EmbeddableProduct): number[] {
  return createEmbedding([product.name, product.category, product.description, ...product.tags].join(" "));
}

export function createQueryEmbedding(query: string): number[] {
  return createEmbedding(query);
}

function createEmbedding(text: string): number[] {
  const normalizedText = text.toLowerCase();
  const values = FEATURE_KEYWORDS.map((keywords, featureIndex) => {
    const keywordScore = keywords.reduce((score, keyword) => {
      const pattern = new RegExp(`\\b${escapeRegExp(keyword)}\\b`, "g");
      return score + (normalizedText.match(pattern)?.length ?? 0);
    }, 0);

    return keywordScore + hashedSignal(normalizedText, featureIndex);
  });

  return normalize(values);
}

function hashedSignal(text: string, featureIndex: number): number {
  let hash = 17 + featureIndex * 31;

  for (const char of text) {
    hash = (hash * 33 + char.charCodeAt(0) + featureIndex) % 997;
  }

  return (hash % 11) / 100;
}

function normalize(values: number[]): number[] {
  const norm = Math.sqrt(values.reduce((sum, value) => sum + value * value, 0));

  if (norm === 0) {
    return Array.from({ length: EMBEDDING_DIMENSIONS }, () => 0);
  }

  return values.map((value) => Number((value / norm).toFixed(6)));
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}
