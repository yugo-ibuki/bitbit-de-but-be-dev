import { build } from 'esbuild';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const result = await build({ entryPoints: [resolve(root, 'src/main.ts')], bundle: true, minify: true, format: 'iife', write: false, define: { 'process.env.NODE_ENV': '"production"' } });
const [template, css] = await Promise.all([
  readFile(resolve(root, 'index.html'), 'utf8'), readFile(resolve(root, 'src/style.css'), 'utf8')
]);
const js = result.outputFiles[0].text.replace(/<\/script/gi, '<\\/script');
const body = template
  .replace('<link rel="stylesheet" href="/src/style.css">', () => `<style>${css}</style>`)
  .replace('<script type="module" src="/src/main.ts"></script>', () => `<script>${js}</script>`);
await mkdir(resolve(root, 'dist'), { recursive: true });
await writeFile(resolve(root, 'dist/index.html'), body);
console.log(`Built standalone dist/index.html (${Math.round(Buffer.byteLength(body) / 1024)} KiB)`);
