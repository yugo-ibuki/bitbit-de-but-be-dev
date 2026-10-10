import { describe, expect, it } from 'vitest';
import { POWER_THRESHOLDS, Simulation, enemyProfile, shotProfile, waveColumns } from './simulation';

function registerKills(sim: Simulation, count: number) {
  for (let i = 0; i < count; i++) {
    const target = { x: 80 + i % 8 * 48, y: 180 };
    sim.addEnemy(target);
    sim.bullets.push({ id: -i - 1, ...target, vx: 0, vy: 0, damage: 99 });
    sim.step(.001);
  }
}

function hitEnemy(sim: Simulation, x: number, y: number, damage: number) {
  sim.bullets.push({ id: -1000 - sim.bullets.length, x, y, vx: 0, vy: 0, damage });
  return sim.step(.001);
}

describe('Orbit Breaker simulation', () => {
  it('enemy collision awards score and removes destroyed enemy', () => {
    const sim = new Simulation(); sim.start();
    sim.addEnemy({ x: sim.player.x, y: sim.player.y - 80, hp: 1, maxHp: 1 });
    for (let i = 0; i < 20; i++) sim.step(.05);
    expect(sim.score).toBe(100); expect(sim.enemies.every(enemy => enemy.hp > 0)).toBe(true);
  });
  it('a hit consumes one life, then invulnerability prevents repeated damage', () => {
    const sim = new Simulation(); sim.start();
    sim.addEnemy({ ...sim.player }); sim.step(.016);
    expect(sim.lives).toBe(2); expect(sim.invulnerable).toBeGreaterThan(1);
    sim.addEnemy({ ...sim.player }); sim.step(.016);
    expect(sim.lives).toBe(2);
  });
  it('three separated hits end the run', () => {
    const sim = new Simulation(); sim.start();
    for (let hit = 0; hit < 3; hit++) {
      sim.addEnemy({ ...sim.player }); sim.step(.016);
      if (hit < 2) { for (let i = 0; i < 33; i++) { sim.enemies = []; sim.step(.05); } }
    }
    expect(sim.lives).toBe(0); expect(sim.status).toBe('lost');
  });
  it('pause freezes timers and explicit resume continues them', () => {
    const sim = new Simulation(); sim.start(); sim.step(1);
    const elapsed = sim.elapsed; sim.pause(); sim.step(1);
    expect(sim.elapsed).toBe(elapsed); sim.resume(); sim.step(.02); expect(sim.elapsed).toBeGreaterThan(elapsed);
  });
  it('kill thresholds raise power and even levels restore at most one shield', () => {
    const sim = new Simulation(); sim.start(); sim.lives = 2;
    registerKills(sim, POWER_THRESHOLDS[1]);
    expect(sim.level).toBe(2); expect(sim.kills).toBe(6); expect(sim.lives).toBe(3);
    registerKills(sim, POWER_THRESHOLDS[2] - POWER_THRESHOLDS[1]);
    expect(sim.level).toBe(3);
  });
  it('fire patterns gain lanes, rate and damage with power', () => {
    expect(shotProfile(1)).toMatchObject({ lanes: [0], damage: 1, interval: .18 });
    expect(shotProfile(4).lanes).toHaveLength(5);
    expect(shotProfile(4).damage).toBe(2);
    expect(shotProfile(6).lanes).toHaveLength(7);
    expect(shotProfile(8).damage).toBe(3);
    expect(shotProfile(11).damage).toBe(4);
    expect(shotProfile(14).damage).toBe(5);
    expect(shotProfile(8).interval).toBeLessThan(shotProfile(1).interval);
  });
  it('difficulty squeezes early, eases at level four, and spikes from level six', () => {
    expect(enemyProfile(0, 0, 1)).toEqual({ maxHp: 4, speedY: 185 });
    expect(enemyProfile(1, 0, 1)).toEqual({ maxHp: 5, speedY: 225 });
    expect(enemyProfile(0, 15, 4)).toEqual({ maxHp: 2, speedY: 160 });
    expect(enemyProfile(0, 15, 6)).toEqual({ maxHp: 5, speedY: 230 });
    expect(enemyProfile(0, 30, 8)).toEqual({ maxHp: 7, speedY: 275 });
    expect(enemyProfile(2, 30, 8)).toEqual({ maxHp: 21, speedY: 175 });
    expect(enemyProfile(3, 30, 8)).toEqual({ maxHp: 44, speedY: 125 });
    expect(enemyProfile(3, 60, 15)).toEqual(enemyProfile(3, 60, 8));
  });
  it('late formations reserve a visibly wider escape column', () => {
    const columns = waveColumns(7, 3);
    const gaps = columns.slice(1).map((value, index) => value - columns[index]);
    expect(columns).toHaveLength(7); expect(Math.max(...gaps)).toBeGreaterThan(.2);
  });
  it('homing missiles stay locked below level eight and launch in pairs at level eight', () => {
    const sim = new Simulation(); sim.start();
    for (let i = 0; i < 20; i++) sim.step(.05); expect(sim.missiles).toHaveLength(0);
    sim.level = 8; const launch = sim.step(.01); expect(launch.missilesFired).toBe(2); expect(sim.missiles).toHaveLength(2);
    for (let i = 0; i < 15; i++) sim.step(.05); expect(sim.missiles.length).toBeGreaterThanOrEqual(2);
  });
  it('a missile curves toward a side target instead of snapping to it', () => {
    const sim = new Simulation(); sim.start(); sim.level = 8;
    sim.addEnemy({ x: 460, y: 260, hp: 100, maxHp: 100, speedY: 0 }); sim.step(.01);
    const missile = sim.missiles[0], initialVx = missile.vx;
    for (let i = 0; i < 8; i++) sim.step(.05);
    expect(missile.vx).toBeGreaterThan(initialVx); expect(missile.targetId).toBe(sim.enemies[0].id);
  });
  it('missiles retarget after a target is destroyed', () => {
    const sim = new Simulation(); sim.start(); sim.level = 8;
    sim.addEnemy({ x: 120, y: 220, hp: 100, maxHp: 100, speedY: 0 });
    sim.addEnemy({ x: 420, y: 220, hp: 100, maxHp: 100, speedY: 0 }); sim.step(.01);
    const missile = sim.missiles[0], firstTarget = missile.targetId!;
    sim.enemies.find(enemy => enemy.id === firstTarget)!.hp = 0; sim.step(.01);
    expect(missile.targetId).not.toBe(firstTarget); expect(missile.targetId).not.toBeNull();
  });
  it('overlapping missiles award a kill and score only once', () => {
    const sim = new Simulation(); sim.start(); sim.level = 7; const target = { x: 120, y: 180 };
    sim.addEnemy({ ...target, hp: 5, maxHp: 5, speedY: 0 });
    sim.missiles.push(
      { id: -1, ...target, vx: 0, vy: -450, speed: 450, ttl: 1, damage: 5, targetId: null },
      { id: -2, ...target, vx: 0, vy: -450, speed: 450, ttl: 1, damage: 5, targetId: null }
    );
    sim.step(.001); expect(sim.kills).toBe(1); expect(sim.score).toBe(100);
  });
  it('pause freezes missiles, expiry removes them, and restart clears them', () => {
    const sim = new Simulation(); sim.start(); sim.level = 8; sim.step(.01);
    const missile = sim.missiles[0], snapshot = { x: missile.x, y: missile.y, ttl: missile.ttl };
    sim.pause(); sim.step(1); expect({ x: missile.x, y: missile.y, ttl: missile.ttl }).toEqual(snapshot);
    sim.resume(); missile.ttl = .01; sim.step(.05); expect(sim.missiles.includes(missile)).toBe(false);
    sim.start(); expect(sim.missiles).toHaveLength(0);
  });
  it('the beam stays locked through level fourteen and fires immediately at level fifteen', () => {
    const sim = new Simulation(); sim.start(); sim.level = 14;
    expect(sim.step(.01).beamsFired).toBe(0); expect(sim.beamRemaining).toBe(0);
    sim.level = 15; expect(sim.step(.01).beamsFired).toBe(1); expect(sim.beamRemaining).toBeGreaterThan(.9);
  });
  it('the beam repeats every five active seconds', () => {
    const sim = new Simulation(); sim.start(); sim.level = 15; expect(sim.step(.01).beamsFired).toBe(1);
    let fired = 0; for (let i = 0; i < 99; i++) fired += sim.step(.05).beamsFired;
    expect(fired).toBe(0); fired += sim.step(.05).beamsFired; expect(fired).toBe(1);
  });
  it('the beam pierces multiple enemies inside its width and misses enemies outside', () => {
    const sim = new Simulation(); sim.start(); sim.level = 15;
    sim.addEnemy({ x: 200, y: 180, hp: 100, maxHp: 100, speedY: 0 });
    sim.addEnemy({ x: 340, y: 260, hp: 100, maxHp: 100, speedY: 0 });
    sim.addEnemy({ x: 90, y: 220, hp: 100, maxHp: 100, speedY: 0 }); sim.step(.05);
    expect(sim.enemies[0].hp).toBeCloseTo(96.5); expect(sim.enemies[1].hp).toBeCloseTo(96.5); expect(sim.enemies[2].hp).toBe(100);
  });
  it('laser and beam overlap still awards one kill', () => {
    const sim = new Simulation(); sim.start(); sim.level = 15;
    sim.addEnemy({ x: sim.player.x, y: sim.player.y - 38, hp: 1, maxHp: 1, speedY: 0 });
    const event = sim.step(.01); expect(event.kills).toHaveLength(1); expect(sim.kills).toBe(1); expect(sim.score).toBe(100);
  });
  it('pause freezes beam state and restart clears its cycle', () => {
    const sim = new Simulation(); sim.start(); sim.level = 15; sim.step(.01);
    const state = { remaining: sim.beamRemaining, cooldown: sim.beamCooldown, center: sim.beamCenterX };
    sim.pause(); sim.step(1); expect({ remaining: sim.beamRemaining, cooldown: sim.beamCooldown, center: sim.beamCenterX }).toEqual(state);
    sim.start(); expect(sim.beamRemaining).toBe(0); expect(sim.beamCooldown).toBe(0);
  });
  it('power progression caps at level fifteen', () => {
    const sim = new Simulation(); sim.start(); registerKills(sim, POWER_THRESHOLDS.at(-1)!);
    expect(sim.level).toBe(15); expect(sim.kills).toBe(480); expect(sim.nextPowerAt()).toBe(480);
  });
  it('heavy and armored types stay out of early waves and appear later', () => {
    const sim = new Simulation(); sim.start(0x51f15e); sim.invulnerable = 999;
    const earlyKinds = new Set<number>();
    for (let i = 0; i < 180; i++) { sim.step(.05); for (const enemy of sim.enemies) earlyKinds.add(enemy.kind); }
    expect(earlyKinds.has(2)).toBe(false); expect(earlyKinds.has(3)).toBe(false);
    const laterKinds = new Set<number>();
    for (let i = 0; i < 500; i++) { sim.step(.05); for (const enemy of sim.enemies) laterKinds.add(enemy.kind); }
    expect(laterKinds.has(2)).toBe(true); expect(laterKinds.has(3)).toBe(true);
  });
  it('heavy enemies absorb hits and award progress only on the final hit', () => {
    const sim = new Simulation(); sim.start(); sim.addEnemy({ x: 100, y: 180, kind: 2, hp: 4, maxHp: 4, speedY: 0 });
    hitEnemy(sim, 100, 180, 1); expect(sim.enemies[0].hp).toBe(3); expect(sim.kills).toBe(0); expect(sim.score).toBe(0);
    hitEnemy(sim, 100, 180, 1); hitEnemy(sim, 100, 180, 1); const final = hitEnemy(sim, 100, 180, 1);
    expect(sim.kills).toBe(1); expect(sim.score).toBe(300); expect(sim.combo).toBe(1); expect(final.kills).toHaveLength(1);
  });
  it('late armored enemies still need several maximum-power hits', () => {
    const sim = new Simulation(); sim.start(); sim.elapsed = 59;
    const profile = enemyProfile(3, sim.elapsed); sim.addEnemy({ x: 270, y: 200, kind: 3, speedY: 0 });
    const hitsNeeded = Math.ceil(profile.maxHp / 3);
    for (let i = 0; i < hitsNeeded - 1; i++) hitEnemy(sim, 270, 200, 3);
    expect(sim.kills).toBe(0); expect(sim.enemies[0].hp).toBeGreaterThan(0);
    hitEnemy(sim, 270, 200, 3); expect(sim.kills).toBe(1); expect(sim.score).toBe(700);
  });
  it('combo expires after two active seconds but freezes while paused', () => {
    const sim = new Simulation(); sim.start(); registerKills(sim, 2);
    expect(sim.combo).toBe(2); const remaining = sim.comboTimer;
    sim.pause(); sim.step(1); expect(sim.comboTimer).toBe(remaining);
    sim.resume(); for (let i = 0; i < 41; i++) { sim.enemies = []; sim.step(.05); }
    expect(sim.combo).toBe(0);
  });
  it('restart clears transient entities and restores lives', () => {
    const sim = new Simulation(); sim.start(); sim.addEnemy({ x: 10, y: 10 }); sim.step(.05); sim.start(7);
    expect(sim.enemies).toHaveLength(0); expect(sim.bullets).toHaveLength(0); expect(sim.missiles).toHaveLength(0); expect(sim.lives).toBe(3); expect(sim.score).toBe(0);
    expect(sim.level).toBe(1); expect(sim.kills).toBe(0); expect(sim.combo).toBe(0);
  });
  it('large waves stay within entity caps and horizontal play bounds', () => {
    const sim = new Simulation(); sim.start(); sim.invulnerable = 999;
    for (let i = 0; i < 400; i++) sim.step(.05, { x: 0, y: 0 });
    expect(sim.enemies.length).toBeLessThanOrEqual(110); expect(sim.bullets.length).toBeLessThanOrEqual(280);
    expect(sim.enemies.every(enemy => enemy.x >= 24 && enemy.x <= 516)).toBe(true);
  });
  it('surviving 60 seconds wins even with bounded frame deltas', () => {
    const sim = new Simulation(); sim.start();
    for (let i = 0; i < 1201; i++) { sim.enemies = []; sim.step(.05); }
    expect(sim.status).toBe('won'); expect(sim.elapsed).toBe(60);
  });
});
