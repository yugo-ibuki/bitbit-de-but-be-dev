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
    const next = stepGame(state(), () => 0);

    expect(next.snake).toEqual([
      { x: 6, y: 5 },
      { x: 5, y: 5 },
      { x: 4, y: 5 },
    ]);
  });

  it("grows and increases the score after eating food", () => {
    const next = stepGame(state({ food: { x: 6, y: 5 } }), () => 0);

    expect(next.snake).toHaveLength(4);
    expect(next.score).toBe(10);
    expect(next.food).toEqual({ x: 0, y: 0 });
  });

  it("ends the game when the head hits a wall", () => {
    const next = stepGame(state({ snake: [{ x: 19, y: 5 }] }), () => 0);

    expect(next.status).toBe("gameover");
  });

  it("ends the game when the head hits the snake body", () => {
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
      () => 0,
    );

    expect(next.status).toBe("gameover");
  });
});

describe("changeDirection", () => {
  it("ignores a direction opposite to current movement", () => {
    const next = changeDirection(state(), "left");

    expect(next.queuedDirection).toBe("right");
  });
});

describe("placeFood", () => {
  it("skips occupied cells", () => {
    const food = placeFood(2, [{ x: 0, y: 0 }], () => 0);

    expect(food).toEqual({ x: 1, y: 0 });
  });
});

describe("restartGame", () => {
  it("restores the initial score, length, direction and ready state", () => {
    const next = restartGame(state({ score: 80, status: "gameover" }), () => 0.7);
    const initial = createGame(() => 0.7);

    expect(next).toEqual(initial);
    expect(next.status).toBe("ready");
  });
});
