import { describe, expect, it } from "vitest";
import {
  changeDirection,
  createGame,
  placeFood,
  restartGame,
  stepGame,
  type GameState,
} from "./game";

const state = (overrides: Partial<GameState> = {}): GameState => ({
  boardSize: 20,
  snake: [
    { x: 5, y: 5 },
    { x: 4, y: 5 },
    { x: 3, y: 5 },
  ],
  food: { x: 10, y: 10 },
  direction: "right",
  queuedDirection: "right",
  score: 0,
  status: "running",
  ...overrides,
});

describe("stepGame", () => {
  it("moves the head one cell and removes the tail", () => {
    expect(stepGame(state(), () => 0).snake).toEqual([
      { x: 6, y: 5 },
      { x: 5, y: 5 },
      { x: 4, y: 5 },
    ]);
  });

  it("grows and adds ten points after eating food", () => {
    const next = stepGame(state({ food: { x: 6, y: 5 } }), () => 0);

    expect(next.snake).toHaveLength(4);
    expect(next.score).toBe(10);
    expect(next.food).toEqual({ x: 0, y: 0 });
  });

  it("ends the game at a wall", () => {
    expect(stepGame(state({ snake: [{ x: 19, y: 5 }] })).status).toBe(
      "gameover",
    );
  });

  it("ends the game on self collision", () => {
    const next = stepGame(
      state({
        snake: [
          { x: 5, y: 5 },
          { x: 5, y: 6 },
          { x: 6, y: 6 },
          { x: 6, y: 5 },
          { x: 7, y: 5 },
        ],
      }),
    );

    expect(next.status).toBe("gameover");
  });

  it("does not move while paused", () => {
    const paused = state({ status: "paused" });
    expect(stepGame(paused)).toBe(paused);
  });
});

describe("changeDirection", () => {
  it("ignores the opposite of the current movement", () => {
    expect(changeDirection(state(), "left").queuedDirection).toBe("right");
  });
});

describe("placeFood", () => {
  it("uses a free cell and never an occupied cell", () => {
    expect(placeFood(2, [{ x: 0, y: 0 }], () => 0)).toEqual({ x: 1, y: 0 });
  });

  it("returns null when the board is full", () => {
    expect(
      placeFood(
        2,
        [
          { x: 0, y: 0 },
          { x: 1, y: 0 },
          { x: 0, y: 1 },
          { x: 1, y: 1 },
        ],
        () => 0,
      ),
    ).toBeNull();
  });
});

describe("restartGame", () => {
  it("restores score, length, direction, and ready state", () => {
    const next = restartGame(state({ score: 80, status: "gameover" }), () => 0.7);
    expect(next).toEqual(createGame(() => 0.7));
    expect(next.status).toBe("ready");
  });
});
