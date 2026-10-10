export const WORLD = { width: 540, height: 820 } as const;
export const POWER_THRESHOLDS = [0, 6, 16, 28, 50, 85, 125, 175, 215, 255, 300, 345, 390, 435, 480] as const;
export type Status = 'menu' | 'playing' | 'paused' | 'won' | 'lost';
export type Vec = { x: number; y: number };
export type Bullet = Vec & { id: number; vx: number; vy: number; damage: number };
export type Missile = Vec & { id: number; vx: number; vy: number; speed: number; ttl: number; damage: number; targetId: number | null };
export type EnemyKind = 0 | 1 | 2 | 3;
export type Enemy = Vec & { id: number; kind: EnemyKind; hp: number; maxHp: number; speedY: number; hitFlash: number; age: number; baseX: number; phase: number };
export type Input = { x: number; y: number; pointer?: Vec | null };
export type KillEvent = Vec & { kind: Enemy['kind']; score: number; combo: number };
export type Events = { shots: number; missilesFired: number; beamsFired: number; kills: KillEvent[]; hit: boolean; ended: boolean; levelUps: number[] };

const BULLET_CAP = 280;
const ENEMY_CAP = 110;
const MISSILE_CAP = 24;

export class Simulation {
  status: Status = 'menu'; score = 0; lives = 3; elapsed = 0; invulnerable = 0;
  kills = 0; level = 1; combo = 0; comboTimer = 0;
  beamRemaining = 0; beamCooldown = 0; beamCenterX = WORLD.width / 2;
  player: Vec = { x: WORLD.width / 2, y: WORLD.height - 92 };
  bullets: Bullet[] = []; missiles: Missile[] = []; enemies: Enemy[] = [];
  private id = 1; private shootTimer = 0; private missileTimer = 0; private waveTimer = 0; private seed = 0x51f15e;

  start(seed = 0x51f15e) {
    this.status = 'playing'; this.score = 0; this.lives = 3; this.elapsed = 0; this.invulnerable = 0;
    this.kills = 0; this.level = 1; this.combo = 0; this.comboTimer = 0; this.beamRemaining = 0; this.beamCooldown = 0; this.beamCenterX = WORLD.width / 2;
    this.player = { x: WORLD.width / 2, y: WORLD.height - 92 };
    this.bullets = []; this.missiles = []; this.enemies = []; this.id = 1; this.shootTimer = 0; this.missileTimer = 0; this.waveTimer = .18; this.seed = seed;
  }
  pause() { if (this.status === 'playing') this.status = 'paused'; }
  resume() { if (this.status === 'paused') this.status = 'playing'; }
  togglePause() { this.status === 'playing' ? this.pause() : this.resume(); }
  nextPowerAt() { return POWER_THRESHOLDS[this.level] ?? POWER_THRESHOLDS.at(-1)!; }

  step(rawDt: number, input: Input = { x: 0, y: 0 }): Events {
    const events: Events = { shots: 0, missilesFired: 0, beamsFired: 0, kills: [], hit: false, ended: false, levelUps: [] };
    if (this.status !== 'playing') return events;
    const dt = Math.min(Math.max(rawDt, 0), .05);
    this.elapsed += dt; this.invulnerable = Math.max(0, this.invulnerable - dt);
    this.comboTimer = Math.max(0, this.comboTimer - dt); if (this.comboTimer === 0) this.combo = 0;
    this.movePlayer(dt, input);

    this.shootTimer -= dt;
    if (this.shootTimer <= 0 && this.bullets.length < BULLET_CAP) {
      const profile = shotProfile(this.level);
      for (const vx of profile.lanes) {
        if (this.bullets.length >= BULLET_CAP) break;
        this.bullets.push({ id: this.id++, x: this.player.x, y: this.player.y - 31, vx, vy: -700, damage: profile.damage }); events.shots++;
      }
      this.shootTimer += profile.interval;
    }
    if (this.level >= 8) {
      this.missileTimer -= dt;
      if (this.missileTimer <= 0 && this.missiles.length <= MISSILE_CAP - 2) {
        this.spawnMissiles(); events.missilesFired = 2; this.missileTimer += .72;
      }
    }
    if (this.level >= 15) {
      this.beamCooldown -= dt;
      if (this.beamRemaining <= 0 && this.beamCooldown <= 0) {
        this.beamRemaining = 1; this.beamCooldown += 5; events.beamsFired = 1;
      }
      this.beamCenterX = clamp(this.player.x, WORLD.width / 6, WORLD.width * 5 / 6);
    }
    this.waveTimer -= dt;
    if (this.waveTimer <= 0) {
      this.spawnWave();
      const difficultyLevel = Math.min(this.level, 8);
      const stageInterval = difficultyLevel <= 3 ? .48 : difficultyLevel <= 5 ? .56 : .43 - (difficultyLevel - 6) * .03;
      this.waveTimer += stageInterval + this.rand() * .07;
    }
    for (const b of this.bullets) { b.x += b.vx * dt; b.y += b.vy * dt; }
    for (const missile of this.missiles) this.updateMissile(missile, dt);
    for (const e of this.enemies) {
      e.age += dt; e.hitFlash = Math.max(0, e.hitFlash - dt); e.y += e.speedY * dt;
      if (e.kind === 1) e.x = clamp(e.baseX + Math.sin(e.age * 3.2 + e.phase) * 42, 24, WORLD.width - 24);
      if (e.kind === 2) e.x = clamp(e.baseX + Math.sin(e.age * 1.7 + e.phase) * 60, 34, WORLD.width - 34);
    }
    this.resolveBulletHits(events); this.resolveMissileHits(events);
    if (this.beamRemaining > 0) this.resolveBeamHits(events, Math.min(dt, this.beamRemaining));
    this.resolvePlayerHit(events);
    this.beamRemaining = Math.max(0, this.beamRemaining - dt);
    this.bullets = this.bullets.filter(b => b.y > -50 && b.x > -40 && b.x < WORLD.width + 40);
    this.missiles = this.missiles.filter(m => m.ttl > 0 && m.y > -100 && m.y < WORLD.height + 100 && m.x > -80 && m.x < WORLD.width + 80);
    this.enemies = this.enemies.filter(e => e.hp > 0 && e.y < WORLD.height + 70);
    if (this.elapsed >= 60 && this.status === 'playing') { this.elapsed = 60; this.status = 'won'; events.ended = true; }
    return events;
  }

  /** Test/debug boundary: injects an entity without exposing renderer objects. */
  addEnemy(enemy: Partial<Enemy> & Vec) {
    const kind = enemy.kind ?? 0, profile = enemyProfile(kind, this.elapsed, this.level), hp = enemy.hp ?? profile.maxHp;
    this.enemies.push({ id: this.id++, kind, hp, maxHp: enemy.maxHp ?? hp, speedY: enemy.speedY ?? profile.speedY, hitFlash: 0, age: 0, baseX: enemy.x, phase: 0, ...enemy });
  }
  private movePlayer(dt: number, input: Input) {
    const speed = 350;
    if (input.pointer) {
      const dx = input.pointer.x - this.player.x, dy = input.pointer.y - this.player.y, d = Math.hypot(dx, dy), travel = speed * 1.45 * dt;
      if (d > 2) { this.player.x += dx / d * Math.min(d, travel); this.player.y += dy / d * Math.min(d, travel); }
    } else {
      const d = Math.hypot(input.x, input.y) || 1; this.player.x += input.x / d * speed * dt; this.player.y += input.y / d * speed * dt;
    }
    this.player.x = clamp(this.player.x, 28, WORLD.width - 28); this.player.y = clamp(this.player.y, 92, WORLD.height - 34);
  }
  private resolveBulletHits(events: Events) {
    for (const b of this.bullets) {
      if (b.y <= -50) continue;
      for (const e of this.enemies) {
        if (e.hp <= 0 || distSq(b, e) >= enemyRadius(e.kind) ** 2) continue;
        b.y = -100; e.hp -= b.damage; e.hitFlash = .1; if (e.hp <= 0) this.registerKill(e, events); break;
      }
    }
  }
  private resolveMissileHits(events: Events) {
    for (const missile of this.missiles) {
      if (missile.ttl <= 0) continue;
      for (const enemy of this.enemies) {
        if (enemy.hp <= 0 || distSq(missile, enemy) >= (enemyRadius(enemy.kind) + 8) ** 2) continue;
        missile.ttl = 0; enemy.hp -= missile.damage; enemy.hitFlash = .14;
        if (enemy.hp <= 0) this.registerKill(enemy, events);
        break;
      }
    }
  }
  private resolveBeamHits(events: Events, dt: number) {
    const halfWidth = WORLD.width / 6, top = -60, bottom = this.player.y - 38;
    for (const enemy of this.enemies) {
      const radius = enemyRadius(enemy.kind);
      if (enemy.hp <= 0 || enemy.y - radius > bottom || enemy.y + radius < top || Math.abs(enemy.x - this.beamCenterX) > halfWidth + radius) continue;
      enemy.hp -= 70 * dt; enemy.hitFlash = .08;
      if (enemy.hp <= 0) this.registerKill(enemy, events);
    }
  }
  private registerKill(enemy: Enemy, events: Events) {
    this.kills++; this.combo = this.comboTimer > 0 ? this.combo + 1 : 1; this.comboTimer = 2;
    const multiplier = Math.min(5, 1 + Math.floor((this.combo - 1) / 5));
    const points = (enemy.kind === 3 ? 700 : enemy.kind === 2 ? 300 : enemy.kind === 1 ? 180 : 100) * multiplier;
    this.score += points; events.kills.push({ x: enemy.x, y: enemy.y, kind: enemy.kind, score: points, combo: this.combo });
    while (this.level < POWER_THRESHOLDS.length && this.kills >= POWER_THRESHOLDS[this.level]) {
      this.level++; if (this.level % 2 === 0) this.lives = Math.min(3, this.lives + 1); events.levelUps.push(this.level);
    }
  }
  private resolvePlayerHit(events: Events) {
    if (this.invulnerable !== 0) return;
    const hit = this.enemies.find(e => e.hp > 0 && distSq(e, this.player) < enemyContactRadius(e.kind) ** 2);
    if (!hit) return;
    hit.hp = 0; this.lives--; this.invulnerable = 1.6; events.hit = true;
    if (this.lives <= 0) { this.status = 'lost'; events.ended = true; }
  }
  private spawnMissiles() {
    const targets = this.liveTargetsFrom(this.player);
    for (let wing = 0; wing < 2; wing++) {
      const direction = wing === 0 ? -1 : 1, angle = -Math.PI / 2 + direction * .12, target = targets[wing % Math.max(1, targets.length)];
      this.missiles.push({ id: this.id++, x: this.player.x + direction * 27, y: this.player.y - 6, vx: Math.cos(angle) * 450, vy: Math.sin(angle) * 450, speed: 450, ttl: 3.8, damage: 5, targetId: target?.id ?? null });
    }
  }
  private updateMissile(missile: Missile, dt: number) {
    missile.ttl -= dt;
    let target = this.enemies.find(enemy => enemy.id === missile.targetId && enemy.hp > 0);
    if (!target) { target = this.liveTargetsFrom(missile)[0]; missile.targetId = target?.id ?? null; }
    const current = Math.atan2(missile.vy, missile.vx), desired = target ? Math.atan2(target.y - missile.y, target.x - missile.x) : -Math.PI / 2;
    const turn = clamp(normalizeAngle(desired - current), -3.6 * dt, 3.6 * dt), angle = current + turn;
    missile.vx = Math.cos(angle) * missile.speed; missile.vy = Math.sin(angle) * missile.speed;
    missile.x += missile.vx * dt; missile.y += missile.vy * dt;
  }
  private liveTargetsFrom(origin: Vec) {
    return this.enemies.filter(enemy => enemy.hp > 0 && enemy.y < this.player.y + 70)
      .sort((a, b) => distSq(origin, a) - distSq(origin, b));
  }
  private spawnWave() {
    const remaining = ENEMY_CAP - this.enemies.length; if (remaining <= 0) return;
    const pressure = Math.min(this.elapsed / 60, 1), difficultyLevel = Math.min(this.level, 8);
    const baseCount = difficultyLevel <= 3 ? 5 : difficultyLevel <= 5 ? 4 : 6 + Math.floor((difficultyLevel - 6) / 2);
    const count = Math.min(remaining, difficultyLevel <= 3 ? baseCount : Math.floor(baseCount + this.rand() * 3));
    const pattern = Math.floor(this.rand() * 3);
    const gapSlot = difficultyLevel >= 6 ? Math.floor(this.rand() * (count + 1)) : -1;
    const columns = waveColumns(count, gapSlot);
    for (let i = 0; i < count; i++) {
      const t = columns[i], x = 42 + t * (WORLD.width - 84);
      const yOffset = pattern === 1 ? Math.abs(t - .5) * 90 : pattern === 2 ? Math.sin(t * Math.PI) * -45 : (i % 2) * -24;
      const roll = this.rand();
      const armorChance = difficultyLevel >= 6 ? .06 + (difficultyLevel - 6) * .015 : this.elapsed >= 20 ? .05 + (this.elapsed - 20) / 40 * .07 : 0;
      const heavyChance = difficultyLevel >= 6 ? .12 + (difficultyLevel - 6) * .015 : this.elapsed >= 10 ? .1 + pressure * .04 : 0;
      const agileChance = difficultyLevel <= 3 ? .3 : .25;
      const kind: EnemyKind = roll < armorChance ? 3 : roll < armorChance + heavyChance ? 2 : roll < armorChance + heavyChance + agileChance ? 1 : 0;
      const profile = enemyProfile(kind, this.elapsed, difficultyLevel);
      this.enemies.push({ id: this.id++, x, y: -40 + yOffset, baseX: x, kind, hp: profile.maxHp, maxHp: profile.maxHp, speedY: profile.speedY, hitFlash: 0, age: 0, phase: this.rand() * Math.PI * 2 });
    }
  }
  private rand() { this.seed = (this.seed * 1664525 + 1013904223) >>> 0; return this.seed / 4294967296; }
}

export function shotProfile(level: number) {
  const lanes = level >= 6 ? [-240, -160, -80, 0, 80, 160, 240] : level >= 4 ? [-170, -85, 0, 85, 170] : level >= 2 ? [-100, 0, 100] : [0];
  const interval = [.18, .155, .135, .11, .098, .088, .076, .065][clamp(Math.floor(level), 1, 8) - 1];
  const damage = level >= 14 ? 5 : level >= 11 ? 4 : level >= 7 ? 3 : level >= 4 ? 2 : 1;
  return { lanes, interval, damage };
}

export function enemyProfile(kind: EnemyKind, elapsed: number, level = 1) {
  level = clamp(level, 1, 8);
  const progress = clamp(elapsed / 60, 0, 1);
  if (level >= 6) {
    const spike = clamp(level - 6, 0, 2);
    if (kind === 3) return { maxHp: 24 + spike * 10, speedY: 90 + spike * 17.5 };
    if (kind === 2) return { maxHp: 13 + spike * 4, speedY: 130 + spike * 22.5 };
    if (kind === 1) return { maxHp: 7 + spike * 2, speedY: 265 + spike * 25 };
    return { maxHp: 5 + spike, speedY: 230 + spike * 22.5 };
  }
  if (level <= 3) {
    if (kind === 3) return { maxHp: 12, speedY: 85 };
    if (kind === 2) return { maxHp: 6, speedY: 125 };
    if (kind === 1) return { maxHp: 5 + Math.floor(progress * 2), speedY: 225 + progress * 20 };
    return { maxHp: 4 + Math.floor(progress * 1.5), speedY: 185 + progress * 20 };
  }
  // LV4〜5は火力が敵の成長を追い越す、短い押し返し区間。
  if (kind === 3) return { maxHp: 14, speedY: 85 };
  if (kind === 2) return { maxHp: 7, speedY: 120 };
  if (kind === 1) return { maxHp: 4, speedY: 195 };
  return { maxHp: 2, speedY: 160 };
}

export const enemyRadius = (kind: EnemyKind) => kind === 3 ? 42 : kind === 2 ? 32 : 24;
const enemyContactRadius = (kind: EnemyKind) => kind === 3 ? 50 : kind === 2 ? 42 : 33;
export function waveColumns(count: number, gapSlot = -1) {
  if (count <= 1) return [.5];
  const hasGap = gapSlot >= 0 && gapSlot <= count;
  const slots = hasGap ? count + 1 : count;
  return Array.from({ length: count }, (_, i) => {
    const slot = hasGap && i >= gapSlot ? i + 1 : i;
    return slot / (slots - 1);
  });
}

const clamp = (v: number, min: number, max: number) => Math.max(min, Math.min(max, v));
const distSq = (a: Vec, b: Vec) => (a.x - b.x) ** 2 + (a.y - b.y) ** 2;
const normalizeAngle = (angle: number) => Math.atan2(Math.sin(angle), Math.cos(angle));
