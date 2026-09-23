import './style.css'
import { Application, Container, Graphics, Text } from 'pixi.js'
import {
  COLS,
  ENEMY_START,
  EXIT,
  PICKUPS,
  PLAYER_START,
  ROWS,
  TILE,
  WORLD_HEIGHT,
  WORLD_WIDTH,
  blockedCells,
  cellCenter,
  collectNearby,
  directionForInput,
  findPath,
  isExitUnlocked,
  moveWithCollisions,
  movementDirection,
  outcome,
  pointCell,
  type GamePhase,
  type Point,
  usesAutomaticAim,
} from './game'

const requireElement = <T extends HTMLElement>(selector: string): T => {
  const element = document.querySelector<T>(selector)
  if (!element) throw new Error(`Missing required element: ${selector}`)
  return element
}

const gameHost = requireElement<HTMLDivElement>('#game')
const overlay = requireElement<HTMLDivElement>('#overlay')
const overlayKicker = requireElement<HTMLParagraphElement>('#overlay-kicker')
const overlayTitle = requireElement<HTMLHeadingElement>('#overlay-title')
const overlayCopy = requireElement<HTMLParagraphElement>('#overlay-copy')
const startButton = requireElement<HTMLButtonElement>('#start')
const pauseButton = requireElement<HTMLButtonElement>('#pause')
const restartButton = requireElement<HTMLButtonElement>('#restart')
const progress = requireElement<HTMLElement>('#progress')
const objective = requireElement<HTMLElement>('#objective')
const message = requireElement<HTMLDivElement>('#message')

const app = new Application()
await app.init({
  background: '#071011',
  antialias: true,
  autoDensity: true,
  resolution: Math.min(window.devicePixelRatio, 1.75),
  resizeTo: gameHost,
})
gameHost.appendChild(app.canvas)

const world = new Container()
app.stage.addChild(world)

const floorLayer = new Graphics()
const propLayer = new Container()
const actorLayer = new Container()
const darkness = new Graphics()
const lightBeam = new Graphics()
world.addChild(floorLayer, propLayer, actorLayer, darkness, lightBeam)

function drawFacility() {
  floorLayer.clear().rect(0, 0, WORLD_WIDTH, WORLD_HEIGHT).fill('#10191a')
  for (let row = 0; row < ROWS; row += 1) {
    for (let col = 0; col < COLS; col += 1) {
      const x = col * TILE
      const y = row * TILE
      const blocked = blockedCells.has(`${col},${row}`)
      if (blocked) {
        floorLayer.rect(x + 2, y + 2, TILE - 4, TILE - 4).fill(row === 0 || row === ROWS - 1 || col === 0 || col === COLS - 1 ? '#263132' : '#20292a')
        floorLayer.rect(x + 5, y + 5, TILE - 10, 4).fill({ color: '#5d6c6a', alpha: 0.28 })
        floorLayer.rect(x + 5, y + TILE - 9, TILE - 10, 3).fill({ color: '#000000', alpha: 0.45 })
      } else {
        const tone = (row + col) % 2 === 0 ? '#111b1c' : '#0e1819'
        floorLayer.rect(x + 1, y + 1, TILE - 2, TILE - 2).fill(tone)
        if ((row * 7 + col * 11) % 13 === 0) floorLayer.circle(x + 17, y + 31, 6).fill({ color: '#5a261d', alpha: 0.24 })
      }
    }
  }

  floorLayer.rect(TILE, TILE * 4 + 20, TILE * 4, 5).fill({ color: '#d8d1a2', alpha: 0.13 })
  floorLayer.rect(TILE * 10 + 20, TILE * 8, 5, TILE * 5).fill({ color: '#d8d1a2', alpha: 0.12 })
}

drawFacility()

const exit = new Container()
const exitGlow = new Graphics()
const exitDoor = new Graphics()
exit.addChild(exitGlow, exitDoor)
exit.position.set(EXIT.x, EXIT.y)
propLayer.addChild(exit)

const pickupViews = PICKUPS.map((pickup, index) => {
  const container = new Container()
  const glow = new Graphics().circle(0, 0, 24).fill({ color: '#71ffc1', alpha: 0.10 })
  const body = new Graphics()
    .roundRect(-8, -14, 16, 28, 3)
    .fill('#9affd2')
    .rect(-4, -9, 8, 12)
    .fill('#183e31')
    .rect(-5, 8, 3, 8)
    .rect(2, 8, 3, 8)
    .fill('#b8ffdf')
  container.addChild(glow, body)
  container.position.set(pickup.x, pickup.y)
  const label = new Text({ text: `FUSE 0${index + 1}`, style: { fontFamily: 'monospace', fontSize: 8, fill: '#b8ffe0', letterSpacing: 1 } })
  label.anchor.set(0.5)
  label.y = 25
  container.addChild(label)
  propLayer.addChild(container)
  return container
})

const playerView = new Container()
const playerShadow = new Graphics().ellipse(3, 8, 17, 10).fill({ color: '#000000', alpha: 0.55 })
const playerBody = new Graphics()
  .circle(0, 0, 13).fill('#d9e4de')
  .circle(0, 0, 8).fill('#52645e')
  .circle(0, -2, 3).fill('#dfffee')
const flashlight = new Graphics().circle(15, 0, 4).fill('#eafff4')
playerView.addChild(playerShadow, playerBody, flashlight)
actorLayer.addChild(playerView)

const enemyView = new Container()
const enemyAura = new Graphics().circle(0, 0, 32).fill({ color: '#7d001d', alpha: 0.16 })
const enemyBody = new Graphics()
  .ellipse(0, 3, 17, 24).fill({ color: '#020203', alpha: 0.96 })
  .circle(-6, -5, 2).fill('#b30031')
  .circle(6, -5, 2).fill('#b30031')
enemyView.addChild(enemyAura, enemyBody)
actorLayer.addChild(enemyView)

let phase: GamePhase = 'intro'
let player: Point = { ...PLAYER_START }
let enemy: Point = { ...ENEMY_START }
let collected = new Set<number>()
let aim = { x: 1, y: 0 }
let manualAim = false
let elapsed = 0
let pathTimer = 0
let enemyPath: Point[] = []
let messageTimer = 0
const pressed = new Set<string>()
const IMMEDIATE_MOVE_DISTANCE = 16

function resetControls() {
  pressed.clear()
  aim = { x: 1, y: 0 }
  manualAim = false
}

function applyDirectionInput(input: string, immediate: boolean) {
  const direction = directionForInput(input)
  if (!direction) return
  if (usesAutomaticAim(input) || !manualAim) aim = direction
  if (usesAutomaticAim(input)) manualAim = false
  if (immediate && phase === 'playing') {
    player = moveWithCollisions(player, {
      x: direction.x * IMMEDIATE_MOVE_DISTANCE,
      y: direction.y * IMMEDIATE_MOVE_DISTANCE,
    })
  }
}

const showMessage = (copy: string) => {
  message.textContent = copy
  message.classList.add('show')
  messageTimer = 2.2
}

function updateHud() {
  progress.textContent = `${collected.size} / ${PICKUPS.length}`
  const unlocked = isExitUnlocked(collected.size, PICKUPS.length)
  objective.innerHTML = unlocked
    ? '<span class="pulse-dot"></span> 非常口へ向かえ — 北東区画'
    : `<span class="pulse-dot"></span> ヒューズを探せ — 残り ${PICKUPS.length - collected.size}`
  exitGlow.clear().circle(0, 0, 31).fill({ color: unlocked ? '#72ffc0' : '#ff364e', alpha: unlocked ? 0.18 : 0.10 })
  exitDoor.clear()
    .rect(-18, -22, 36, 44).fill(unlocked ? '#245e47' : '#482027')
    .rect(-13, -17, 26, 34).stroke({ color: unlocked ? '#96ffd0' : '#c65362', width: 2, alpha: 0.8 })
    .circle(9, 1, 2).fill(unlocked ? '#d8ffee' : '#ff7180')
}

function resetGame() {
  player = { ...PLAYER_START }
  enemy = { ...ENEMY_START }
  collected = new Set()
  elapsed = 0
  pathTimer = 0
  enemyPath = []
  resetControls()
  pickupViews.forEach((view) => { view.visible = true })
  updateHud()
}

function begin() {
  if (phase === 'won' || phase === 'lost') resetGame()
  else if (phase === 'intro') resetControls()
  phase = 'playing'
  pauseButton.disabled = false
  pauseButton.textContent = 'PAUSE'
  pauseButton.setAttribute('aria-label', 'ゲームを一時停止')
  overlay.classList.add('hidden')
  showMessage('SIGNAL FOUND // 3 FUSES REQUIRED')
}

function finish(next: 'won' | 'lost') {
  phase = next
  pauseButton.disabled = true
  overlayKicker.textContent = next === 'won' ? 'EMERGENCY POWER RESTORED' : 'SIGNAL LOST // SUBJECT UNRESPONSIVE'
  overlayTitle.textContent = next === 'won' ? 'YOU MADE IT OUT.' : 'IT FOUND YOU.'
  overlayCopy.textContent = next === 'won'
    ? '非常口のロックが解除された。夜が明けるまで、振り返るな。'
    : '光が届くより先に、暗闇が追いついた。もう一度施設へ戻る。'
  startButton.textContent = 'TRY AGAIN'
  overlay.classList.remove('hidden')
}

function pauseToggle() {
  if (phase === 'playing') {
    phase = 'paused'
    pauseButton.textContent = 'RESUME'
    pauseButton.setAttribute('aria-label', 'ゲームを再開')
    overlayKicker.textContent = 'TRANSMISSION SUSPENDED'
    overlayTitle.textContent = 'HOLD YOUR BREATH.'
    overlayCopy.textContent = '追跡は止まっている。準備ができたら施設へ戻れ。'
    startButton.textContent = 'RESUME'
    overlay.classList.remove('hidden')
  } else if (phase === 'paused') begin()
}

function updateEnemy(deltaSeconds: number) {
  if (elapsed < 3.5) return
  pathTimer -= deltaSeconds
  if (pathTimer <= 0) {
    const path = findPath(pointCell(enemy), pointCell(player))
    enemyPath = path.slice(1).map(cellCenter)
    pathTimer = 0.32
  }
  const target = enemyPath[0]
  if (!target) return
  const dx = target.x - enemy.x
  const dy = target.y - enemy.y
  const length = Math.hypot(dx, dy)
  if (length < 4) {
    enemyPath.shift()
    return
  }
  const speed = collected.size === PICKUPS.length ? 88 : 68 + collected.size * 5
  const movement = Math.min(length, speed * deltaSeconds)
  enemy.x += dx / length * movement
  enemy.y += dy / length * movement
}

function updateDarkness(now: number) {
  const flicker = Math.sin(now * 0.017) * 4 + Math.sin(now * 0.041) * 2
  const angle = Math.atan2(aim.y, aim.x)
  const reach = 300 + flicker
  const spread = 0.48
  darkness.clear().rect(0, 0, WORLD_WIDTH, WORLD_HEIGHT).fill({ color: '#010304', alpha: 0.70 })
  darkness.circle(player.x, player.y, 88 + flicker).cut()
  const beamPoints = [
    player.x, player.y,
    player.x + Math.cos(angle - spread) * reach, player.y + Math.sin(angle - spread) * reach,
    player.x + Math.cos(angle) * (reach + 42), player.y + Math.sin(angle) * (reach + 42),
    player.x + Math.cos(angle + spread) * reach, player.y + Math.sin(angle + spread) * reach,
  ]
  darkness.poly(beamPoints).cut()
  lightBeam.clear().poly(beamPoints).fill({ color: '#b6ffe0', alpha: 0.075 })
}

function resizeWorld() {
  const scale = Math.min(app.screen.width / WORLD_WIDTH, app.screen.height / WORLD_HEIGHT)
  world.scale.set(scale)
  world.position.set((app.screen.width - WORLD_WIDTH * scale) / 2, (app.screen.height - WORLD_HEIGHT * scale) / 2)
}

const resizeObserver = new ResizeObserver(resizeWorld)
resizeObserver.observe(gameHost)
resizeWorld()
updateHud()

app.ticker.add((ticker) => {
  const deltaSeconds = Math.min(ticker.deltaMS / 1000, 0.04)
  if (phase === 'playing') {
    elapsed += deltaSeconds
    const movement = movementDirection(pressed)
    if (movement) {
      if (!manualAim) aim = movement
      player = moveWithCollisions(player, { x: movement.x * 168 * deltaSeconds, y: movement.y * 168 * deltaSeconds })
    }

    const nextCollected = collectNearby(player, PICKUPS, collected)
    if (nextCollected.size > collected.size) {
      const added = [...nextCollected].find((index) => !collected.has(index))
      collected = nextCollected
      if (added !== undefined) pickupViews[added].visible = false
      updateHud()
      showMessage(collected.size === PICKUPS.length ? 'ALL FUSES ONLINE // EXIT UNLOCKED' : `FUSE RECOVERED // ${collected.size} OF 3`)
    }

    updateEnemy(deltaSeconds)
    const result = outcome(player, enemy, EXIT, isExitUnlocked(collected.size, PICKUPS.length))
    if (result !== 'playing') finish(result)
  }

  if (messageTimer > 0) {
    messageTimer -= deltaSeconds
    if (messageTimer <= 0) message.classList.remove('show')
  }

  const now = performance.now()
  playerView.position.set(player.x, player.y)
  playerView.rotation = Math.atan2(aim.y, aim.x)
  enemyView.position.set(enemy.x, enemy.y)
  enemyView.scale.set(1 + Math.sin(now * 0.004) * 0.06)
  pickupViews.forEach((view, index) => {
    view.y = PICKUPS[index].y + Math.sin(now * 0.003 + index) * 3
    view.alpha = 0.78 + Math.sin(now * 0.006 + index) * 0.2
  })
  exit.alpha = 0.8 + Math.sin(now * 0.005) * 0.18
  updateDarkness(now)
})

window.addEventListener('keydown', (event) => {
  const key = event.key.toLowerCase()
  const direction = directionForInput(key)
  if (direction) {
    event.preventDefault()
    const isNewPress = !event.repeat && !pressed.has(key)
    applyDirectionInput(key, isNewPress)
    pressed.add(key)
    return
  }
  if (key === 'p') pauseToggle()
})
window.addEventListener('keyup', (event) => pressed.delete(event.key.toLowerCase()))
window.addEventListener('blur', () => pressed.clear())

app.canvas.addEventListener('pointermove', (event) => {
  if (phase !== 'playing' || [...pressed].some(usesAutomaticAim)) return
  const bounds = app.canvas.getBoundingClientRect()
  const screenX = (event.clientX - bounds.left) * (app.screen.width / bounds.width)
  const screenY = (event.clientY - bounds.top) * (app.screen.height / bounds.height)
  const worldX = (screenX - world.x) / world.scale.x
  const worldY = (screenY - world.y) / world.scale.y
  const dx = worldX - player.x
  const dy = worldY - player.y
  const length = Math.hypot(dx, dy)
  if (length > 1) {
    aim = { x: dx / length, y: dy / length }
    manualAim = true
  }
})

document.querySelectorAll<HTMLButtonElement>('[data-direction]').forEach((button) => {
  const direction = button.dataset.direction!
  const press = (event: PointerEvent) => {
    event.preventDefault()
    button.setPointerCapture(event.pointerId)
    const isNewPress = !pressed.has(direction)
    applyDirectionInput(direction, isNewPress)
    pressed.add(direction)
  }
  const release = (event: PointerEvent) => {
    event.preventDefault()
    pressed.delete(direction)
  }
  button.addEventListener('pointerdown', press)
  button.addEventListener('pointerup', release)
  button.addEventListener('pointercancel', release)
  button.addEventListener('lostpointercapture', () => pressed.delete(direction))
})

startButton.addEventListener('click', begin)
pauseButton.addEventListener('click', pauseToggle)
restartButton.addEventListener('click', () => {
  resetGame()
  phase = 'playing'
  pauseButton.disabled = false
  pauseButton.textContent = 'PAUSE'
  pauseButton.setAttribute('aria-label', 'ゲームを一時停止')
  overlay.classList.add('hidden')
  showMessage('SYSTEM RESET // MOVE')
})

window.addEventListener('beforeunload', () => {
  resizeObserver.disconnect()
  app.destroy(true, { children: true })
})
