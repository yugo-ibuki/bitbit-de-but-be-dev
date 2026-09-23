export const COLS = 20
export const ROWS = 14
export const TILE = 48
export const WORLD_WIDTH = COLS * TILE
export const WORLD_HEIGHT = ROWS * TILE

export type Point = { x: number; y: number }
export type Cell = { col: number; row: number }
export type GamePhase = 'intro' | 'playing' | 'paused' | 'won' | 'lost'

const INPUT_DIRECTIONS: Readonly<Record<string, Point>> = {
  arrowleft: { x: -1, y: 0 },
  a: { x: -1, y: 0 },
  left: { x: -1, y: 0 },
  arrowright: { x: 1, y: 0 },
  d: { x: 1, y: 0 },
  right: { x: 1, y: 0 },
  arrowup: { x: 0, y: -1 },
  w: { x: 0, y: -1 },
  up: { x: 0, y: -1 },
  arrowdown: { x: 0, y: 1 },
  s: { x: 0, y: 1 },
  down: { x: 0, y: 1 },
}

export function directionForInput(input: string): Point | null {
  return INPUT_DIRECTIONS[input.toLowerCase()] ?? null
}

export function movementDirection(inputs: Iterable<string>): Point | null {
  let left = false
  let right = false
  let up = false
  let down = false
  for (const input of inputs) {
    const direction = directionForInput(input)
    if (direction) {
      if (direction.x < 0) left = true
      if (direction.x > 0) right = true
      if (direction.y < 0) up = true
      if (direction.y > 0) down = true
    }
  }
  const x = Number(right) - Number(left)
  const y = Number(down) - Number(up)
  const length = Math.hypot(x, y)
  return length === 0 ? null : { x: x / length, y: y / length }
}

export const usesAutomaticAim = (input: string) => {
  const normalized = input.toLowerCase()
  return normalized.startsWith('arrow') || ['left', 'right', 'up', 'down'].includes(normalized)
}

const key = ({ col, row }: Cell) => `${col},${row}`

export const blockedCells = (() => {
  const cells = new Set<string>()
  for (let col = 0; col < COLS; col += 1) {
    cells.add(key({ col, row: 0 }))
    cells.add(key({ col, row: ROWS - 1 }))
  }
  for (let row = 0; row < ROWS; row += 1) {
    cells.add(key({ col: 0, row }))
    cells.add(key({ col: COLS - 1, row }))
  }
  for (let row = 1; row <= 10; row += 1) if (row !== 4) cells.add(key({ col: 5, row }))
  for (let row = 3; row <= 12; row += 1) if (row !== 8) cells.add(key({ col: 10, row }))
  for (let row = 1; row <= 10; row += 1) if (row !== 5) cells.add(key({ col: 15, row }))
  for (let col = 6; col <= 9; col += 1) if (col !== 8) cells.add(key({ col, row: 6 }))
  for (let col = 11; col <= 14; col += 1) if (col !== 13) cells.add(key({ col, row: 10 }))
  return cells
})()

export const isBlocked = (cell: Cell) =>
  cell.col < 0 || cell.row < 0 || cell.col >= COLS || cell.row >= ROWS || blockedCells.has(key(cell))

export const cellCenter = ({ col, row }: Cell): Point => ({
  x: (col + 0.5) * TILE,
  y: (row + 0.5) * TILE,
})

export const pointCell = ({ x, y }: Point): Cell => ({
  col: Math.floor(x / TILE),
  row: Math.floor(y / TILE),
})

export function collidesWithWall(point: Point, radius: number): boolean {
  const minCol = Math.floor((point.x - radius) / TILE)
  const maxCol = Math.floor((point.x + radius) / TILE)
  const minRow = Math.floor((point.y - radius) / TILE)
  const maxRow = Math.floor((point.y + radius) / TILE)

  for (let row = minRow; row <= maxRow; row += 1) {
    for (let col = minCol; col <= maxCol; col += 1) {
      if (!isBlocked({ col, row })) continue
      const nearestX = Math.max(col * TILE, Math.min(point.x, (col + 1) * TILE))
      const nearestY = Math.max(row * TILE, Math.min(point.y, (row + 1) * TILE))
      if ((point.x - nearestX) ** 2 + (point.y - nearestY) ** 2 < radius ** 2) return true
    }
  }
  return false
}

export function moveWithCollisions(position: Point, delta: Point, radius = 14): Point {
  const nextX = { x: position.x + delta.x, y: position.y }
  const afterX = collidesWithWall(nextX, radius) ? position : nextX
  const nextY = { x: afterX.x, y: afterX.y + delta.y }
  return collidesWithWall(nextY, radius) ? afterX : nextY
}

export function findPath(start: Cell, goal: Cell): Cell[] {
  if (isBlocked(start) || isBlocked(goal)) return []
  const queue: Cell[] = [start]
  const previous = new Map<string, Cell | null>([[key(start), null]])
  const directions = [{ col: 1, row: 0 }, { col: -1, row: 0 }, { col: 0, row: 1 }, { col: 0, row: -1 }]

  while (queue.length > 0) {
    const current = queue.shift()!
    if (current.col === goal.col && current.row === goal.row) {
      const path: Cell[] = []
      let cursor: Cell | null = current
      while (cursor) {
        path.unshift(cursor)
        cursor = previous.get(key(cursor)) ?? null
      }
      return path
    }
    for (const direction of directions) {
      const next = { col: current.col + direction.col, row: current.row + direction.row }
      if (isBlocked(next) || previous.has(key(next))) continue
      previous.set(key(next), current)
      queue.push(next)
    }
  }
  return []
}

export const distance = (a: Point, b: Point) => Math.hypot(a.x - b.x, a.y - b.y)

export function collectNearby(player: Point, pickups: readonly Point[], collected: ReadonlySet<number>, range = 30) {
  const next = new Set(collected)
  pickups.forEach((pickup, index) => {
    if (distance(player, pickup) <= range) next.add(index)
  })
  return next
}

export const isExitUnlocked = (collectedCount: number, total: number) => collectedCount === total

export function outcome(player: Point, enemy: Point, exit: Point, unlocked: boolean): 'playing' | 'won' | 'lost' {
  if (distance(player, enemy) < 29) return 'lost'
  if (unlocked && distance(player, exit) < 34) return 'won'
  return 'playing'
}

export const PICKUPS = [cellCenter({ col: 17, row: 2 }), cellCenter({ col: 2, row: 11 }), cellCenter({ col: 12, row: 7 })]
export const PLAYER_START = cellCenter({ col: 2, row: 2 })
export const ENEMY_START = cellCenter({ col: 17, row: 11 })
export const EXIT = cellCenter({ col: 18, row: 1 })
