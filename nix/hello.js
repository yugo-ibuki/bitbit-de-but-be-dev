#!/usr/bin/env node

const name = process.argv[2] ?? "Nix";

console.log(`Hello, ${name}!`);
console.log(`Node.js: ${process.version}`);
console.log(`Platform: ${process.platform} ${process.arch}`);
console.log("Use `nix run` or `nix develop` to select the Node.js pinned by this flake.");
