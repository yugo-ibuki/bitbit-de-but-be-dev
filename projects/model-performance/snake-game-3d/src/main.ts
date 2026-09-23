import "./style.css";
import * as THREE from "three";
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
const boardFrame = requireElement<HTMLDivElement>("#board-frame");
const score = requireElement<HTMLElement>("#score");
const length = requireElement<HTMLElement>("#length");
const status = requireElement<HTMLElement>("#status");
const overlay = requireElement<HTMLDivElement>("#overlay");
const overlayKicker = requireElement<HTMLElement>("#overlay-kicker");
const overlayTitle = requireElement<HTMLElement>("#overlay-title");
const overlayDetail = requireElement<HTMLElement>("#overlay-detail");
const webglError = requireElement<HTMLDivElement>("#webgl-error");
const startButton = requireElement<HTMLButtonElement>("#start-button");
const startIcon = requireElement<HTMLElement>("#start-icon");
const startLabel = requireElement<HTMLElement>("#start-label");
const restartButton = requireElement<HTMLButtonElement>("#restart-button");
const directionButtons = document.querySelectorAll<HTMLButtonElement>(
  "[data-direction]",
);

const BOARD_SIZE = 20;
const CELL_SIZE = 1;
const BOARD_CENTER = (BOARD_SIZE - 1) / 2;
const sharedGeometries: THREE.BufferGeometry[] = [];
const sharedMaterials: THREE.Material[] = [];
let renderer: THREE.WebGLRenderer | null = null;
let scene: THREE.Scene | null = null;
let camera: THREE.OrthographicCamera | null = null;
let dynamicGroup: THREE.Group | null = null;
let foodMesh: THREE.Mesh | null = null;
let resizeObserver: ResizeObserver | null = null;
let webglAvailable = true;

interface RenderAssets {
  segmentGeometry: THREE.BoxGeometry;
  bodyMaterial: THREE.MeshStandardMaterial;
  headMaterial: THREE.MeshStandardMaterial;
  headingGeometry: THREE.ConeGeometry;
  headingMaterial: THREE.MeshBasicMaterial;
  foodGeometry: THREE.IcosahedronGeometry;
  foodMaterial: THREE.MeshStandardMaterial;
}

let renderAssets: RenderAssets | null = null;

const rememberGeometry = <T extends THREE.BufferGeometry>(geometry: T): T => {
  sharedGeometries.push(geometry);
  return geometry;
};

const rememberMaterial = <T extends THREE.Material>(material: T): T => {
  sharedMaterials.push(material);
  return material;
};

const getRenderAssets = (): RenderAssets => {
  if (renderAssets) {
    return renderAssets;
  }
  renderAssets = {
    segmentGeometry: rememberGeometry(
      new THREE.BoxGeometry(0.78, 0.72, 0.78, 2, 2, 2),
    ),
    bodyMaterial: rememberMaterial(
      new THREE.MeshStandardMaterial({
        color: 0x31dca4,
        emissive: 0x0d563f,
        emissiveIntensity: 0.75,
        roughness: 0.3,
        metalness: 0.35,
      }),
    ),
    headMaterial: rememberMaterial(
      new THREE.MeshStandardMaterial({
        color: 0xc7ff62,
        emissive: 0x4c7415,
        emissiveIntensity: 1.15,
        roughness: 0.22,
        metalness: 0.24,
      }),
    ),
    headingGeometry: rememberGeometry(new THREE.ConeGeometry(0.15, 0.44, 3)),
    headingMaterial: rememberMaterial(
      new THREE.MeshBasicMaterial({ color: 0x07120e }),
    ),
    foodGeometry: rememberGeometry(new THREE.IcosahedronGeometry(0.43, 1)),
    foodMaterial: rememberMaterial(
      new THREE.MeshStandardMaterial({
        color: 0xff4f79,
        emissive: 0xb30e42,
        emissiveIntensity: 1.8,
        roughness: 0.24,
        metalness: 0.28,
      }),
    ),
  };
  return renderAssets;
};

const toWorld = (x: number, y: number): THREE.Vector3 =>
  new THREE.Vector3(
    (x - BOARD_CENTER) * CELL_SIZE,
    0.48,
    (y - BOARD_CENTER) * CELL_SIZE,
  );

const resizeRenderer = (): void => {
  if (!renderer || !camera) {
    return;
  }
  const width = Math.max(board.clientWidth, 1);
  const height = Math.max(board.clientHeight, 1);
  const aspect = width / height;
  // 斜めから見た盤面と壁が縦横どちらにも収まる視野を確保する。
  const viewSize = Math.max(21, 28 / aspect);
  camera.left = (-viewSize * aspect) / 2;
  camera.right = (viewSize * aspect) / 2;
  camera.top = viewSize / 2;
  camera.bottom = -viewSize / 2;
  camera.updateProjectionMatrix();
  renderer.setSize(width, height, false);
};

const addBoard = (target: THREE.Scene): void => {
  const floorGeometry = rememberGeometry(
    new THREE.PlaneGeometry(BOARD_SIZE, BOARD_SIZE),
  );
  const floorMaterial = rememberMaterial(
    new THREE.MeshStandardMaterial({
      color: 0x091a23,
      roughness: 0.76,
      metalness: 0.16,
    }),
  );
  const floor = new THREE.Mesh(floorGeometry, floorMaterial);
  floor.rotation.x = -Math.PI / 2;
  floor.receiveShadow = true;
  target.add(floor);

  const grid = new THREE.GridHelper(BOARD_SIZE, BOARD_SIZE, 0x62f6cf, 0x173f48);
  grid.position.y = 0.015;
  const gridMaterials = Array.isArray(grid.material)
    ? grid.material
    : [grid.material];
  gridMaterials.forEach((material) => {
    material.transparent = true;
    material.opacity = 0.48;
  });
  target.add(grid);

  const wallGeometry = rememberGeometry(new THREE.BoxGeometry(1, 0.42, 1));
  const wallMaterial = rememberMaterial(
    new THREE.MeshStandardMaterial({
      color: 0x173c47,
      emissive: 0x09252c,
      roughness: 0.38,
      metalness: 0.5,
    }),
  );
  const horizontal = new THREE.InstancedMesh(wallGeometry, wallMaterial, 40);
  const vertical = new THREE.InstancedMesh(wallGeometry, wallMaterial, 40);
  const transform = new THREE.Matrix4();
  for (let index = 0; index < BOARD_SIZE; index += 1) {
    const axis = index - BOARD_CENTER;
    transform.makeTranslation(axis, 0.18, -BOARD_SIZE / 2 - 0.35);
    horizontal.setMatrixAt(index, transform);
    transform.makeTranslation(axis, 0.18, BOARD_SIZE / 2 + 0.35);
    horizontal.setMatrixAt(index + BOARD_SIZE, transform);
    transform.makeTranslation(-BOARD_SIZE / 2 - 0.35, 0.18, axis);
    vertical.setMatrixAt(index, transform);
    transform.makeTranslation(BOARD_SIZE / 2 + 0.35, 0.18, axis);
    vertical.setMatrixAt(index + BOARD_SIZE, transform);
  }
  horizontal.castShadow = true;
  vertical.castShadow = true;
  horizontal.receiveShadow = true;
  vertical.receiveShadow = true;
  target.add(horizontal, vertical);
};

const initializeThree = (): void => {
  try {
    renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 1.75));
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    renderer.outputColorSpace = THREE.SRGBColorSpace;
    renderer.domElement.setAttribute("aria-hidden", "true");
    board.replaceChildren(renderer.domElement);

    scene = new THREE.Scene();
    scene.fog = new THREE.FogExp2(0x061016, 0.025);
    camera = new THREE.OrthographicCamera(-15, 15, 15, -15, 0.1, 100);
    // 盤面を斜め上から見せ、立体の側面と奥行きが分かる角度にする。
    camera.position.set(7, 24, 24);
    camera.lookAt(0, 0, 0);

    scene.add(new THREE.HemisphereLight(0x9fffe4, 0x091218, 1.7));
    const keyLight = new THREE.DirectionalLight(0xd9fff5, 3.4);
    keyLight.position.set(-8, 20, 10);
    keyLight.castShadow = true;
    keyLight.shadow.mapSize.set(1024, 1024);
    keyLight.shadow.camera.left = -16;
    keyLight.shadow.camera.right = 16;
    keyLight.shadow.camera.top = 16;
    keyLight.shadow.camera.bottom = -16;
    keyLight.shadow.camera.near = 1;
    keyLight.shadow.camera.far = 50;
    scene.add(keyLight);

    const accentLight = new THREE.PointLight(0xff5e8a, 16, 30, 2);
    accentLight.position.set(8, 7, -7);
    scene.add(accentLight);

    addBoard(scene);
    dynamicGroup = new THREE.Group();
    scene.add(dynamicGroup);

    resizeObserver = new ResizeObserver(resizeRenderer);
    resizeObserver.observe(board);
    resizeRenderer();

    const clock = new THREE.Clock();
    renderer.setAnimationLoop(() => {
      if (!renderer || !scene || !camera) {
        return;
      }
      if (foodMesh) {
        const elapsed = clock.getElapsedTime();
        foodMesh.rotation.y = elapsed * 1.4;
        foodMesh.rotation.x = elapsed * 0.55;
        foodMesh.position.y = 0.62 + Math.sin(elapsed * 3) * 0.09;
      }
      renderer.render(scene, camera);
    });
  } catch (error) {
    webglAvailable = false;
    boardFrame.classList.add("board-frame--failed");
    webglError.hidden = false;
    overlay.classList.remove("overlay--visible");
    startButton.disabled = true;
    status.textContent = "3D 表示エラー";
    console.error("WebGL initialization failed", error);
  }
};

let game = createGame();

const renderWorld = (): void => {
  if (!dynamicGroup) {
    return;
  }
  dynamicGroup.clear();
  foodMesh = null;
  const assets = getRenderAssets();

  game.snake.forEach(({ x, y }, index) => {
    const segment = new THREE.Mesh(
      assets.segmentGeometry,
      index === 0 ? assets.headMaterial : assets.bodyMaterial,
    );
    segment.position.copy(toWorld(x, y));
    segment.position.y += index === 0 ? 0.08 : 0;
    segment.scale.setScalar(index === 0 ? 1.08 : Math.max(0.82, 1 - index * 0.012));
    segment.castShadow = true;
    segment.receiveShadow = true;
    dynamicGroup?.add(segment);
  });

  const heading = new THREE.Mesh(
    assets.headingGeometry,
    assets.headingMaterial,
  );
  const directionVector: Record<Direction, THREE.Vector3> = {
    up: new THREE.Vector3(0, 0, -1),
    down: new THREE.Vector3(0, 0, 1),
    left: new THREE.Vector3(-1, 0, 0),
    right: new THREE.Vector3(1, 0, 0),
  };
  const headingDirection = directionVector[game.queuedDirection];
  heading.position.copy(toWorld(game.snake[0].x, game.snake[0].y));
  heading.position.addScaledVector(headingDirection, 0.2);
  heading.position.y = 0.92;
  heading.quaternion.setFromUnitVectors(
    new THREE.Vector3(0, 1, 0),
    headingDirection,
  );
  dynamicGroup.add(heading);

  if (game.food) {
    foodMesh = new THREE.Mesh(assets.foodGeometry, assets.foodMaterial);
    foodMesh.position.copy(toWorld(game.food.x, game.food.y));
    foodMesh.position.y = 0.62;
    foodMesh.castShadow = true;
    dynamicGroup.add(foodMesh);
  }
};

const renderInterface = (): void => {
  score.textContent = game.score.toString().padStart(3, "0");
  length.textContent = game.snake.length.toString().padStart(2, "0");
  board.setAttribute(
    "aria-label",
    `20掛ける20の立体ゲーム盤。上は画面奥、右は画面右方向。スネークの長さ${game.snake.length}、スコア${game.score}。`,
  );

  const stateCopy: Record<GameState["status"], string> = {
    ready: "準備完了",
    running: "プレイ中",
    paused: "一時停止中",
    gameover: "ゲーム終了",
  };
  const nextStatus = webglAvailable ? stateCopy[game.status] : "3D 表示エラー";
  if (status.textContent !== nextStatus) {
    status.textContent = nextStatus;
  }
  status.parentElement?.classList.toggle(
    "status-block--stopped",
    game.status !== "running",
  );

  overlay.classList.toggle(
    "overlay--visible",
    webglAvailable && game.status !== "running",
  );
  if (game.status === "ready") {
    overlayKicker.textContent = "SYSTEM READY";
    overlayTitle.textContent = "ENTER THE GRID";
    overlayDetail.textContent = "START を押してゲーム開始";
  } else if (game.status === "paused") {
    overlayKicker.textContent = "SYSTEM HOLD";
    overlayTitle.textContent = "PAUSED";
    overlayDetail.textContent = "RESUME でゲーム再開";
  } else if (game.status === "gameover") {
    overlayKicker.textContent = "SIGNAL LOST";
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
  startButton.disabled = !webglAvailable || game.status === "gameover";
  startButton.setAttribute(
    "aria-label",
    isRunning ? "ゲームを一時停止" : "ゲームを開始または再開",
  );
};

const render = (): void => {
  renderWorld();
  renderInterface();
};

const togglePlay = (): void => {
  if (!webglAvailable || game.status === "gameover") {
    return;
  }
  game = {
    ...game,
    status: game.status === "running" ? "paused" : "running",
  };
  renderInterface();
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
  } else if (event.code === "Space") {
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

window.addEventListener("beforeunload", () => {
  resizeObserver?.disconnect();
  renderer?.setAnimationLoop(null);
  renderer?.dispose();
  sharedGeometries.forEach((geometry) => geometry.dispose());
  sharedMaterials.forEach((material) => material.dispose());
});

initializeThree();
render();
