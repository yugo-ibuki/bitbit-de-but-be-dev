import type { ChallengePiece, PieceSize, ShapeKind, Vec3 } from './types'

export const PIECE_SCALE: Record<PieceSize, number> = {
  small: 0.78,
  medium: 1,
  large: 1.25,
}

const COLORS = ['#ff6b6b', '#ffd166', '#06d6a0', '#4cc9f0', '#b517ff'] as const

function hashString(value: string): number {
  let hash = 0x811c9dc5
  for (let index = 0; index < value.length; index += 1) {
    hash ^= value.charCodeAt(index)
    hash = Math.imul(hash, 0x01000193)
  }
  return hash >>> 0
}

function createRandom(seed: number): () => number {
  return () => {
    seed = (seed + 0x6d2b79f5) | 0
    let value = Math.imul(seed ^ (seed >>> 15), 1 | seed)
    value ^= value + Math.imul(value ^ (value >>> 7), 61 | value)
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296
  }
}

function shuffle<T>(items: readonly T[], random: () => number): T[] {
  const result = [...items]
  for (let index = result.length - 1; index > 0; index -= 1) {
    const swapIndex = Math.floor(random() * (index + 1))
    ;[result[index], result[swapIndex]] = [result[swapIndex], result[index]]
  }
  return result
}

function repeat<T>(value: T, count: number): T[] {
  return Array.from({ length: count }, () => value)
}

function createPiece(
  kind: ShapeKind,
  size: PieceSize,
  random: () => number,
): ChallengePiece {
  const angle = () => (random() - 0.5) * 0.36
  const rotation: Vec3 = [angle(), angle(), angle()]
  return {
    kind,
    size,
    scale: PIECE_SCALE[size],
    color: COLORS[Math.floor(random() * COLORS.length)],
    rotation,
  }
}

export function getJapanChallengeKey(now = new Date()): string {
  try {
    const parts = new Intl.DateTimeFormat('en', {
      timeZone: 'Asia/Tokyo',
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    }).formatToParts(now)
    const values = Object.fromEntries(parts.map((part) => [part.type, part.value]))
    return `${values.year}-${values.month}-${values.day}`
  } catch {
    return now.toISOString().slice(0, 10)
  }
}

export function createDailyChallengeDeck(key: string): ChallengePiece[] {
  const random = createRandom(hashString(key))
  const kinds = shuffle<ShapeKind>(
    [
      ...repeat<ShapeKind>('box', 33),
      ...repeat<ShapeKind>('sphere', 33),
      ...repeat<ShapeKind>('cylinder', 33),
    ],
    random,
  )
  const sizes = shuffle<PieceSize>(
    [
      ...repeat<PieceSize>('small', 30),
      ...repeat<PieceSize>('medium', 40),
      ...repeat<PieceSize>('large', 29),
    ],
    random,
  )
  return [
    createPiece('box', 'large', random),
    ...kinds.map((kind, index) => createPiece(kind, sizes[index], random)),
  ]
}
