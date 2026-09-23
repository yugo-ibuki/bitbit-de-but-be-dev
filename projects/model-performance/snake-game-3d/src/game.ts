export type Direction = "up" | "down" | "left" | "right";
export type GameStatus = "ready" | "running" | "paused" | "gameover";

export interface Position {
  x: number;
  y: number;
}

export interface GameState {
  boardSize: number;
  snake: Position[];
  food: Position | null;
  direction: Direction;
  queuedDirection: Direction;
  score: number;
  status: GameStatus;
}

export type RandomSource = () => number;

export function placeFood(
  boardSize: number,
  snake: Position[],
  random: RandomSource = Math.random,
): Position | null {
  const occupied = new Set(snake.map(({ x, y }) => `${x},${y}`));
  const freeCells: Position[] = [];

  for (let y = 0; y < boardSize; y += 1) {
    for (let x = 0; x < boardSize; x += 1) {
      if (!occupied.has(`${x},${y}`)) {
        freeCells.push({ x, y });
      }
    }
  }

  if (freeCells.length === 0) {
    return null;
  }

  const index = Math.min(
    Math.floor(random() * freeCells.length),
    freeCells.length - 1,
  );
  return freeCells[index];
}

export function createGame(random: RandomSource = Math.random): GameState {
  const snake = [
    { x: 10, y: 10 },
    { x: 9, y: 10 },
    { x: 8, y: 10 },
  ];

  return {
    boardSize: 20,
    snake,
    food: placeFood(20, snake, random),
    direction: "right",
    queuedDirection: "right",
    score: 0,
    status: "ready",
  };
}

export function changeDirection(
  state: GameState,
  direction: Direction,
): GameState {
  const opposite: Record<Direction, Direction> = {
    up: "down",
    down: "up",
    left: "right",
    right: "left",
  };

  return direction === opposite[state.direction]
    ? state
    : { ...state, queuedDirection: direction };
}

export function stepGame(
  state: GameState,
  random: RandomSource = Math.random,
): GameState {
  if (state.status !== "running") {
    return state;
  }

  const offsets: Record<Direction, Position> = {
    up: { x: 0, y: -1 },
    down: { x: 0, y: 1 },
    left: { x: -1, y: 0 },
    right: { x: 1, y: 0 },
  };
  const head = state.snake[0];
  const offset = offsets[state.queuedDirection];
  const nextHead = { x: head.x + offset.x, y: head.y + offset.y };
  const ateFood =
    state.food !== null &&
    nextHead.x === state.food.x &&
    nextHead.y === state.food.y;
  const collisionBody = ateFood ? state.snake : state.snake.slice(0, -1);
  const hitWall =
    nextHead.x < 0 ||
    nextHead.y < 0 ||
    nextHead.x >= state.boardSize ||
    nextHead.y >= state.boardSize;
  const hitSelf = collisionBody.some(
    ({ x, y }) => x === nextHead.x && y === nextHead.y,
  );

  if (hitWall || hitSelf) {
    return {
      ...state,
      direction: state.queuedDirection,
      status: "gameover",
    };
  }

  const nextSnake = [nextHead, ...state.snake];
  if (!ateFood) {
    nextSnake.pop();
  }
  const food = ateFood
    ? placeFood(state.boardSize, nextSnake, random)
    : state.food;

  return {
    ...state,
    snake: nextSnake,
    food,
    direction: state.queuedDirection,
    score: state.score + (ateFood ? 10 : 0),
    status: food === null ? "gameover" : state.status,
  };
}

export function restartGame(
  _state: GameState,
  random: RandomSource = Math.random,
): GameState {
  return createGame(random);
}
