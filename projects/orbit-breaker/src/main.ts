import Phaser from 'phaser';
import { POWER_THRESHOLDS, Simulation, WORLD, type Enemy, type Input, type KillEvent } from './simulation';

const $ = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const ui = { overlay: $('overlay'), hud: $('hud'), score: $('score'), time: $('time'), lives: $('lives'),
  level: $('level'), kills: $('kills'), powerFill: $('power-fill'),
  message: $('message'), result: $('result'), primary: $('primary') as HTMLButtonElement, best: $('best'),
  pause: $('pause') as HTMLButtonElement, sound: $('sound') as HTMLButtonElement, toast: $('toast') };
const sim = new Simulation();
let best = loadBest(), muted = false, audio: AudioContext | null = null;
const reducedMotion = matchMedia('(prefers-reduced-motion: reduce)').matches;
const REWARD_INTENSITY = 1.4;
document.documentElement.style.setProperty('--reward-intensity', String(REWARD_INTENSITY));
type Burst = { x: number; y: number; life: number; max: number; color: number };
let toastTimer = 0;
let toastAnimation: Animation | null = null;
let sceneRef: GameScene | null = null;
ui.best.textContent = `BEST ${pad(best)}`;

class GameScene extends Phaser.Scene {
  private world!: Phaser.GameObjects.Graphics;
  private fx!: Phaser.GameObjects.Graphics;
  private keys!: Record<string, Phaser.Input.Keyboard.Key>;
  private pointerTarget: { x: number; y: number } | null = null;
  private stars: { x: number; y: number; r: number; a: number }[] = [];
  private flash = 0;
  private bursts: Burst[] = [];
  private popups = new Set<Phaser.GameObjects.Text>();

  create() {
    sceneRef = this;
    this.world = this.add.graphics(); this.fx = this.add.graphics();
    for (let i = 0; i < 100; i++) this.stars.push({ x: Math.random() * WORLD.width, y: Math.random() * WORLD.height, r: .5 + Math.random() * 1.3, a: .2 + Math.random() * .55 });
    const kb = this.input.keyboard!;
    this.keys = kb.addKeys('W,A,S,D,UP,DOWN,LEFT,RIGHT,P,ESC') as Record<string, Phaser.Input.Keyboard.Key>;
    this.input.on('pointerdown', (p: Phaser.Input.Pointer) => { if (sim.status === 'playing') this.pointerTarget = { x: p.worldX, y: p.worldY }; });
    this.input.on('pointermove', (p: Phaser.Input.Pointer) => { if (p.isDown && sim.status === 'playing') this.pointerTarget = { x: p.worldX, y: p.worldY }; });
    this.input.on('pointerup', () => this.pointerTarget = null);
    kb.on('keydown-P', () => togglePause()); kb.on('keydown-ESC', () => togglePause());
    this.scale.on('leavefullscreen', () => { if (sim.status === 'playing') showPause('画面を離れたため一時停止しました'); });
  }

  update(_: number, deltaMs: number) {
    if (sim.status !== 'playing') this.pointerTarget = null;
    const input: Input = { x: 0, y: 0, pointer: this.pointerTarget };
    if (this.keys.A.isDown || this.keys.LEFT.isDown) input.x--;
    if (this.keys.D.isDown || this.keys.RIGHT.isDown) input.x++;
    if (this.keys.W.isDown || this.keys.UP.isDown) input.y--;
    if (this.keys.S.isDown || this.keys.DOWN.isDown) input.y++;
    if (input.x || input.y) input.pointer = null;
    const events = sim.step(deltaMs / 1000, input);
    if (events.shots) tone(720 + sim.level * 22, .022, .018);
    if (events.missilesFired) tone(310, .1, .045);
    if (events.beamsFired) {
      tone(145, .42, .09);
      if (!reducedMotion) { this.flash = Math.max(this.flash, .12); this.cameras.main.shake(90, .003); }
    }
    if (events.kills.length) {
      tone(180 + Math.min(events.kills.length, 6) * 24, .065, .04);
      for (const kill of events.kills) this.addBurst(kill);
      const highestCombo = events.kills.at(-1)!.combo;
      if (highestCombo >= 5 && highestCombo % 5 === 0) showToast(`${highestCombo} COMBO  ×${Math.min(5, 1 + Math.floor((highestCombo - 1) / 5))}`);
    }
    if (events.levelUps.length) {
      const level = events.levelUps.at(-1)!;
      showToast(level === 15 ? '極太ビーム  LV15' : level === 8 ? '追尾ミサイル  LV8' : level === 6 ? '敵軍強化  LV 6' : `POWER UP  LV ${level}`);
      tone(420 + level * 55, .24 * REWARD_INTENSITY, .1);
      if (!reducedMotion) { this.flash = .22 * REWARD_INTENSITY; this.cameras.main.shake(110 * REWARD_INTENSITY, .006 * REWARD_INTENSITY); }
    }
    if (events.hit) { tone(80, .18, .12); if (!reducedMotion) { this.flash = .2; this.cameras.main.shake(140, .009); } }
    if (events.ended) finish();
    this.draw(deltaMs / 1000); updateHud();
  }

  private draw(dt: number) {
    const g = this.world; g.clear(); g.fillStyle(0x07111f, 1); g.fillRect(0, 0, WORLD.width, WORLD.height);
    const drift = sim.status === 'playing' ? 22 * dt : 0;
    for (const s of this.stars) { s.y = (s.y + drift * s.r) % WORLD.height; g.fillStyle(0xcdefff, s.a); g.fillCircle(s.x, s.y, s.r); }
    g.lineStyle(1, 0x143651, .35);
    for (let y = 100; y < WORLD.height; y += 120) g.lineBetween(0, y, WORLD.width, y);
    if (sim.beamRemaining > 0) this.drawBeam(g);
    for (const b of sim.bullets) {
      const color = b.damage >= 3 ? 0xffe067 : b.damage >= 2 ? 0xff8e55 : 0x63efff;
      g.fillStyle(color, .22); g.fillCircle(b.x, b.y, b.damage >= 2 ? 8 : 5); g.fillStyle(color, 1); g.fillRect(b.x - 2, b.y - 12, 4, 24);
    }
    for (const missile of sim.missiles) this.drawMissile(g, missile.x, missile.y, missile.vx, missile.vy);
    for (const e of sim.enemies) this.drawEnemy(g, e);
    const blink = sim.invulnerable > 0 && Math.floor(sim.invulnerable * 12) % 2 === 0;
    if (!blink) this.drawShip(g, sim.player.x, sim.player.y, sim.level);
    this.drawBursts(g, dt);
    this.fx.clear(); this.flash = Math.max(0, this.flash - dt);
    if (this.flash > 0) { this.fx.fillStyle(0xff733c, Math.min(.28, this.flash)); this.fx.fillRect(0, 0, WORLD.width, WORLD.height); }
  }
  private drawShip(g: Phaser.GameObjects.Graphics, x: number, y: number, level: number) {
    const flame = 38 + Math.min(level * 3, 20);
    g.fillStyle(level >= 5 ? 0xffd45c : 0xff743d, .7); g.fillTriangle(x - 7, y + 20, x + 7, y + 20, x, y + flame);
    g.fillStyle(0x38dce8, 1); g.fillTriangle(x, y - 34, x - 26, y + 25, x, y + 13); g.fillTriangle(x, y - 34, x + 26, y + 25, x, y + 13);
    g.fillStyle(0xe9ffff, 1); g.fillTriangle(x, y - 25, x - 7, y + 13, x + 7, y + 13);
    if (level >= 2) { g.fillStyle(0x7ff7ff, 1); g.fillRect(x - 31, y - 2, 6, 24); g.fillRect(x + 25, y - 2, 6, 24); }
    if (level >= 4) { g.fillStyle(0xffc059, .9); g.fillTriangle(x - 26, y + 11, x - 39, y + 22, x - 23, y + 24); g.fillTriangle(x + 26, y + 11, x + 39, y + 22, x + 23, y + 24); }
    g.lineStyle(level >= 6 ? 3 : 2, level >= 6 ? 0xffda62 : 0x73f5ff, .7); g.strokeCircle(x, y + 2, 31 + Math.min(level, 6));
  }
  private drawBeam(g: Phaser.GameObjects.Graphics) {
    const x = sim.beamCenterX, bottom = sim.player.y - 38, half = WORLD.width / 6;
    g.fillStyle(0x68ffe3, .12); g.fillRect(x - half, 0, half * 2, bottom);
    g.lineStyle(3, 0x76ffe9, .72); g.lineBetween(x - half, 0, x - half, bottom); g.lineBetween(x + half, 0, x + half, bottom);
    g.fillStyle(0xbafff3, .3); g.fillRect(x - 60, 0, 120, bottom);
    g.fillStyle(0xf1fffb, .72); g.fillRect(x - 18, 0, 36, bottom);
    const flow = (sim.elapsed * 260) % 48;
    g.fillStyle(0xffffff, .48);
    for (let y = -48 + flow; y < bottom; y += 48) g.fillRect(x - 72, y, 144, 7);
  }
  private drawEnemy(g: Phaser.GameObjects.Graphics, enemy: Enemy) {
    const { x, y, kind } = enemy;
    const color = enemy.hitFlash > 0 ? 0xffffff : kind === 3 ? 0xb65cff : kind === 2 ? 0xffb24b : kind === 1 ? 0xff6b53 : 0xff8752;
    g.fillStyle(color, 1);
    if (kind === 3) {
      g.fillPoints([{ x: x - 42, y: y - 15 }, { x: x - 24, y: y - 38 }, { x: x + 24, y: y - 38 }, { x: x + 42, y: y - 15 }, { x: x + 30, y: y + 34 }, { x: x - 30, y: y + 34 }], true);
      g.fillStyle(0x431560, 1); g.fillRect(x - 27, y - 19, 54, 29); g.fillStyle(0xf1a7ff, .9); g.fillRect(x - 21, y - 14, 42, 5);
      g.lineStyle(3, 0xe69cff, .8); g.strokeCircle(x, y + 4, 16);
    }
    else if (kind === 2) { g.fillTriangle(x, y + 31, x - 34, y - 20, x + 34, y - 20); g.fillStyle(0x07111f, 1); g.fillCircle(x, y, 10); g.lineStyle(2, 0xffdd76, .85); g.strokeTriangle(x, y + 25, x - 29, y - 16, x + 29, y - 16); }
    else if (kind === 1) { g.fillTriangle(x, y + 26, x - 27, y - 18, x + 27, y - 18); g.fillStyle(0x07111f, 1); g.fillTriangle(x, y + 8, x - 8, y - 10, x + 8, y - 10); }
    else { g.fillTriangle(x, y + 23, x - 22, y - 15, x + 22, y - 15); g.fillStyle(0xffd0a7, 1); g.fillRect(x - 8, y - 12, 16, 4); }
    if (kind >= 2 || enemy.hp < enemy.maxHp) {
      const width = kind === 3 ? 78 : 58;
      const top = kind === 3 ? y + 43 : kind === 2 ? y + 37 : y - 33;
      const ratio = Math.max(0, enemy.hp / enemy.maxHp);
      g.fillStyle(0x020711, .88); g.fillRect(x - width / 2 - 1, top - 1, width + 2, 7);
      g.fillStyle(kind === 3 ? 0xd978ff : 0xffc357, 1); g.fillRect(x - width / 2, top, width * ratio, 5);
    }
  }
  private drawMissile(g: Phaser.GameObjects.Graphics, x: number, y: number, vx: number, vy: number) {
    const length = Math.hypot(vx, vy) || 1, dx = vx / length, dy = vy / length, px = -dy, py = dx;
    g.lineStyle(7, 0x43ffb2, .16); g.lineBetween(x - dx * 7, y - dy * 7, x - dx * 26, y - dy * 26);
    g.lineStyle(2, 0x7effc8, .75); g.lineBetween(x - dx * 5, y - dy * 5, x - dx * 20, y - dy * 20);
    g.fillStyle(0xdffff0, 1); g.fillTriangle(x + dx * 11, y + dy * 11, x - dx * 8 + px * 5, y - dy * 8 + py * 5, x - dx * 8 - px * 5, y - dy * 8 - py * 5);
    g.fillStyle(0x35d99a, 1); g.fillCircle(x - dx * 8, y - dy * 8, 3);
  }
  private addBurst(kill: KillEvent) {
    if (this.bursts.length < 28) {
      const life = kill.kind === 3 ? .48 : .34, color = kill.kind === 3 ? 0xd978ff : kill.kind === 2 ? 0xffd05b : 0xff744d;
      this.bursts.push({ x: kill.x, y: kill.y, life, max: life, color });
    }
    if (kill.combo % 5 === 0 || kill.kind === 2) {
      const milestone = kill.combo % 5 === 0;
      const label = this.add.text(kill.x, kill.y, `+${kill.score}`, { fontFamily: 'monospace', fontSize: milestone ? '21px' : '15px', color: '#ffe09b', fontStyle: 'bold' }).setOrigin(.5).setDepth(3);
      this.popups.add(label);
      this.tweens.add({ targets: label, y: kill.y - (milestone ? 53 : 38), alpha: 0, duration: milestone ? 728 : 520, onComplete: () => { this.popups.delete(label); label.destroy(); } });
    }
  }
  private drawBursts(g: Phaser.GameObjects.Graphics, dt: number) {
    for (const burst of this.bursts) {
      burst.life -= dt; const p = 1 - burst.life / burst.max, radius = 8 + p * 30;
      g.lineStyle(3 - p * 2, burst.color, Math.max(0, 1 - p)); g.strokeCircle(burst.x, burst.y, radius);
      g.fillStyle(burst.color, Math.max(0, .8 - p));
      for (let i = 0; i < 6; i++) { const a = i / 6 * Math.PI * 2; g.fillCircle(burst.x + Math.cos(a) * radius, burst.y + Math.sin(a) * radius, 3 * (1 - p)); }
    }
    this.bursts = this.bursts.filter(b => b.life > 0);
  }
  resetEffects() {
    this.bursts = []; this.flash = 0; this.cameras.main.resetFX();
    for (const popup of this.popups) { this.tweens.killTweensOf(popup); popup.destroy(); }
    this.popups.clear();
  }
}

new Phaser.Game({ type: Phaser.AUTO, parent: 'game', width: WORLD.width, height: WORLD.height,
  backgroundColor: '#07111f', transparent: false, render: { antialias: true, pixelArt: false },
  scale: { mode: Phaser.Scale.FIT, autoCenter: Phaser.Scale.CENTER_BOTH }, scene: GameScene });

ui.primary.addEventListener('click', () => {
  initAudio();
  if (sim.status === 'paused') { sim.resume(); hideOverlay(); return; }
  sim.start(); sceneRef?.resetEffects(); clearToast(); hideOverlay(); updateHud(); tone(330, .12, .06);
});
ui.pause.addEventListener('click', () => togglePause());
ui.sound.addEventListener('click', () => { initAudio(); muted = !muted; ui.sound.textContent = muted ? '音 OFF' : '音 ON'; });
document.addEventListener('visibilitychange', () => { if (document.hidden && sim.status === 'playing') showPause('画面を離れたため一時停止しました'); });
window.addEventListener('blur', () => { if (sim.status === 'playing') showPause('画面を離れたため一時停止しました'); });

function hideOverlay() { ui.overlay.classList.add('hidden'); ui.hud.classList.remove('hidden'); ui.pause.classList.remove('hidden'); ui.result.classList.add('hidden'); }
function togglePause() { if (sim.status === 'playing') showPause('ゲームを一時停止しています'); else if (sim.status === 'paused') { sim.resume(); hideOverlay(); } }
function showPause(message: string) {
  sim.pause(); ui.message.textContent = message; ui.primary.textContent = 'ゲームに戻る'; ui.result.classList.add('hidden');
  ui.overlay.classList.remove('hidden'); ui.pause.classList.add('hidden');
}
function finish() {
  best = Math.max(best, sim.score); saveBest(best); ui.best.textContent = `BEST ${pad(best)}`;
  ui.message.textContent = sim.status === 'won' ? '軌道防衛に成功しました。' : '防衛線が突破されました。';
  ui.result.textContent = `${sim.status === 'won' ? 'MISSION CLEAR' : 'GAME OVER'}  /  SCORE ${pad(sim.score)}`;
  ui.result.classList.remove('hidden'); ui.primary.textContent = 'もう一度出撃'; ui.overlay.classList.remove('hidden'); ui.pause.classList.add('hidden');
  tone(sim.status === 'won' ? 520 : 110, .35, .12);
}
function updateHud() {
  ui.score.textContent = pad(sim.score); ui.time.textContent = Math.max(0, 60 - sim.elapsed).toFixed(1);
  ui.lives.textContent = Array.from({ length: 3 }, (_, i) => i < sim.lives ? '◆' : '◇').join(' '); ui.level.textContent = `LV ${sim.level}`;
  const previous = POWER_THRESHOLDS[sim.level - 1], next = POWER_THRESHOLDS[sim.level];
  if (next === undefined) {
    const beam = sim.beamRemaining > 0 ? 'BEAM ACTIVE' : `BEAM ${Math.max(0, sim.beamCooldown).toFixed(1)}s`;
    ui.kills.textContent = `撃破 ${sim.kills} / MAX POWER / ${beam}`; ui.powerFill.style.width = '100%';
  }
  else {
    ui.kills.textContent = `撃破 ${sim.kills} / 次の強化 ${next}`;
    ui.powerFill.style.width = `${Math.max(0, Math.min(100, (sim.kills - previous) / (next - previous) * 100))}%`;
  }
}
function showToast(message: string) {
  ui.toast.textContent = message; ui.toast.classList.remove('hidden'); window.clearTimeout(toastTimer);
  toastAnimation?.cancel();
  if (!reducedMotion) {
    toastAnimation = ui.toast.animate([
      { transform: 'translateX(-50%) scale(.82)', opacity: .25 },
      { transform: 'translateX(-50%) scale(1.1)', opacity: 1, offset: .42 },
      { transform: 'translateX(-50%) scale(1)', opacity: 1 }
    ], { duration: 260, easing: 'cubic-bezier(.2,.9,.25,1)' });
  }
  toastTimer = window.setTimeout(() => ui.toast.classList.add('hidden'), 900 * REWARD_INTENSITY);
}
function clearToast() { window.clearTimeout(toastTimer); toastAnimation?.cancel(); toastAnimation = null; ui.toast.classList.add('hidden'); ui.toast.textContent = ''; }
function pad(n: number) { return Math.max(0, n).toString().padStart(6, '0'); }
function loadBest() { try { return Number(localStorage.getItem('orbit-breaker-best')) || 0; } catch { return 0; } }
function saveBest(n: number) { try { localStorage.setItem('orbit-breaker-best', String(n)); } catch { /* private storage may be unavailable */ } }
function initAudio() { if (!audio) try { audio = new AudioContext(); } catch { audio = null; } if (audio?.state === 'suspended') void audio.resume(); }
function tone(freq: number, duration: number, volume: number) {
  if (muted || !audio) return; const o = audio.createOscillator(), gain = audio.createGain();
  o.type = 'square'; o.frequency.value = freq; gain.gain.setValueAtTime(volume, audio.currentTime); gain.gain.exponentialRampToValueAtTime(.0001, audio.currentTime + duration);
  o.connect(gain); gain.connect(audio.destination); o.start(); o.stop(audio.currentTime + duration);
}
