import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import vm from 'node:vm';

const html = await readFile(resolve(process.cwd(), 'dist/index.html'), 'utf8');
if (/<(?:script|link)[^>]+(?:src|href)=["'](?:https?:|\/)/i.test(html)) throw new Error('外部またはルート相対アセット参照が残っています');
const scripts = [...html.matchAll(/<script(?:\s[^>]*)?>([\s\S]*?)<\/script>/gi)];
if (scripts.length !== 1) throw new Error(`インライン script は1個必要です（検出: ${scripts.length}）`);
new vm.Script(scripts[0][1], { filename: 'standalone-game.js' });
for (const label of ['出撃する', '矢印キーで移動', '一時停止', 'SCORE', 'SHIELD']) {
  if (!html.includes(label)) throw new Error(`必須UI文言がありません: ${label}`);
}
console.log('Standalone smoke passed: inline assets, script syntax, and required UI labels');
