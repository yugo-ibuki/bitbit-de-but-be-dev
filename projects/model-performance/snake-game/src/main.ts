import "./style.css";
import {
  changeDirection,
  createGame,
  restartGame,
  stepGame,
  type Direction,
  type GameState,
} from "./game";

const requireElement = <T extends Element>(selector: string): T => {
  const element = document.querySelector<T>(selector);
  if (!element) {
    throw new Error(`Missing required element: ${selector}`);
  }
  return element;
};

const board = requireElement<HTMLDivElement>("#board");
const score = requireElement<HTMLElement>("#score");
const length = requireElement<HTMLElement>("#length");
const status = requireElement<HTMLElement>("#status");
const overlay = requireElement<HTMLDivElement>("#overlay");
const overlayKicker = requireElement<HTMLElement>("#overlay-kicker");
const overlayTitle = requireElement<HTMLElement>("#overlay-title");
const overlayDetail = requireElement<HTMLElement>("#overlay-detail");
const startButton = requireElement<HTMLButtonElement>("#start-button");
const startIcon = requireElement<HTMLElement>("#start-icon");
const startLabel = requireElement<HTMLElement>("#start-label");
const restartButton = requireElement<HTMLButtonElement>("#restart-button");
const directionButtons = document.querySelectorAll<HTMLButtonElement>(
  "[data-direction]",
);

const cells: HTMLSpanElement[] = [];
for (let index = 0; index < 400; index += 1) {
  const cell = document.createElement("span");
  cell.className = "cell";
  cell.setAttribute("aria-hidden", "true");
  cells.push(cell);
}
board.replaceChildren(...cells);

let game = createGame();

const cellIndex = (x: number, y: number): number => y * game.boardSize + x;

const render = (): void => {
  for (const cell of cells) {
    cell.className = "cell";
  }

  game.snake.forEach(({ x, y }, index) => {
    const cell = cells[cellIndex(x, y)];
    cell.classList.add(index === 0 ? "cell--head" : "cell--snake");
  });

  if (game.food) {
    cells[cellIndex(game.food.x, game.food.y)].classList.add("cell--food");
  }

  score.textContent = game.score.toString().padStart(3, "0");
  length.textContent = game.snake.length.toString().padStart(2, "0");
  board.setAttribute(
    "aria-label",
    `20掛ける20のゲーム盤。スネークの長さ${game.snake.length}、スコア${game.score}。`,
  );

  const stateCopy: Record<GameState["status"], string> = {
    ready: "準備完了",
    running: "プレイ中",
    paused: "一時停止中",
    gameover: "ゲーム終了",
  };
  if (status.textContent !== stateCopy[game.status]) {
    status.textContent = stateCopy[game.status];
  }
  status.parentElement?.classList.toggle(
    "status-block--stopped",
    game.status !== "running",
  );

  overlay.classList.toggle("overlay--visible", game.status !== "running");
  if (game.status === "ready") {
    overlayKicker.textContent = "READY?";
    overlayTitle.textContent = "ルートを描こう";
    overlayDetail.textContent = "START を押してゲーム開始";
  } else if (game.status === "paused") {
    overlayKicker.textContent = "ON HOLD";
    overlayTitle.textContent = "PAUSED";
    overlayDetail.textContent = "RESUME でゲーム再開";
  } else if (game.status === "gameover") {
    overlayKicker.textContent = "FINISH";
    overlayTitle.textContent = "GAME OVER";
    overlayDetail.textContent = `SCORE ${game.score.toString().padStart(3, "0")} — RESTART でもう一度`;
  }

  const isRunning = game.status === "running";
  startIcon.textContent = isRunning ? "Ⅱ" : "▶";
  startLabel.textContent = isRunning
    ? "PAUSE"
    : game.status === "paused"
      ? "RESUME"
      : "START";
  startButton.disabled = game.status === "gameover";
  startButton.setAttribute(
    "aria-label",
    isRunning ? "ゲームを一時停止" : "ゲームを開始または再開",
  );
};

const togglePlay = (): void => {
  if (game.status === "gameover") {
    return;
  }
  game = {
    ...game,
    status: game.status === "running" ? "paused" : "running",
  };
  render();
};

const restart = (): void => {
  game = restartGame(game);
  render();
};

const steer = (direction: Direction): void => {
  game = changeDirection(game, direction);
};

startButton.addEventListener("click", togglePlay);
restartButton.addEventListener("click", restart);
directionButtons.forEach((button) => {
  button.addEventListener("click", () => {
    steer(button.dataset.direction as Direction);
  });
});

const keyDirections: Record<string, Direction | undefined> = {
  ArrowUp: "up",
  w: "up",
  W: "up",
  ArrowDown: "down",
  s: "down",
  S: "down",
  ArrowLeft: "left",
  a: "left",
  A: "left",
  ArrowRight: "right",
  d: "right",
  D: "right",
};

window.addEventListener("keydown", (event) => {
  const direction = keyDirections[event.key];
  if (direction) {
    event.preventDefault();
    steer(direction);
    return;
  }
  if (event.code === "Space") {
    event.preventDefault();
    togglePlay();
  } else if (event.key === "r" || event.key === "R") {
    restart();
  }
});

window.setInterval(() => {
  if (game.status === "running") {
    game = stepGame(game);
    render();
  }
}, 125);

render();
