import { describe, expect, it } from 'vitest'
import {
  ENEMY_START,
  EXIT,
  PICKUPS,
  PLAYER_START,
  collectNearby,
  collidesWithWall,
  directionForInput,
  findPath,
  isBlocked,
  isExitUnlocked,
  moveWithCollisions,
  movementDirection,
  outcome,
  pointCell,
  usesAutomaticAim,
} from './game'

describe('LAST LIGHT game logic', () => {
  it('blocks the facility boundary and internal partitions', () => {
    expect(isBlocked({ col: 0, row: 4 })).toBe(true)
    expect(isBlocked({ col: 5, row: 3 })).toBe(true)
    expect(isBlocked({ col: 5, row: 4 })).toBe(false)
  })

  it('prevents the player circle from crossing a wall while allowing sliding', () => {
    const besideWall = { x: 5 * 48 - 15, y: 2.5 * 48 }
    const moved = moveWithCollisions(besideWall, { x: 8, y: 9 })
    expect(moved.x).toBe(besideWall.x)
    expect(moved.y).toBeGreaterThan(besideWall.y)
    expect(collidesWithWall(moved, 14)).toBe(false)
  })

  it('collects only pickups inside the interaction range', () => {
    const collected = collectNearby(PICKUPS[0], PICKUPS, new Set())
    expect([...collected]).toEqual([0])
  })

  it('unlocks the exit only after every fuse is collected', () => {
    expect(isExitUnlocked(2, 3)).toBe(false)
    expect(isExitUnlocked(3, 3)).toBe(true)
  })

  it('wins at an unlocked exit and stays in play while it is locked', () => {
    expect(outcome(EXIT, ENEMY_START, EXIT, false)).toBe('playing')
    expect(outcome(EXIT, ENEMY_START, EXIT, true)).toBe('won')
  })

  it('loses when the pursuer reaches the player', () => {
    expect(outcome(PLAYER_START, { ...PLAYER_START }, EXIT, false)).toBe('lost')
  })

  it('finds a navigable path through partition openings', () => {
    const path = findPath(pointCell(ENEMY_START), pointCell(PLAYER_START))
    expect(path.length).toBeGreaterThan(1)
    expect(path.every((cell) => !isBlocked(cell))).toBe(true)
    expect(path.at(-1)).toEqual(pointCell(PLAYER_START))

    for (const objective of [...PICKUPS, EXIT]) {
      expect(findPath(pointCell(PLAYER_START), pointCell(objective)).length).toBeGreaterThan(1)
    }
  })

  it('normalizes combined movement so diagonal holds are not faster', () => {
    expect(movementDirection(['w', 'd'])).toEqual({
      x: 1 / Math.sqrt(2),
      y: -1 / Math.sqrt(2),
    })
    expect(movementDirection(['ArrowRight', 'd', 'w'])).toEqual({
      x: 1 / Math.sqrt(2),
      y: -1 / Math.sqrt(2),
    })
    expect(movementDirection(['a', 'd'])).toBeNull()
  })

  it('separates automatic-aim controls from WASD pointer aiming', () => {
    expect(directionForInput('ArrowRight')).toEqual({ x: 1, y: 0 })
    expect(directionForInput('w')).toEqual({ x: 0, y: -1 })
    expect(usesAutomaticAim('ArrowRight')).toBe(true)
    expect(usesAutomaticAim('right')).toBe(true)
    expect(usesAutomaticAim('d')).toBe(false)
  })
})
