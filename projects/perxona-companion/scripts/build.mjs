import { build } from "esbuild";

await build({
  entryPoints: ["public/avatar.js"],
  outfile: "public/avatar.bundle.js",
  bundle: true,
  format: "esm",
  platform: "browser",
  target: ["es2022"],
  sourcemap: false,
  minify: true,
});
