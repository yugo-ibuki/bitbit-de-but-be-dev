# Local Luma Avatar Implementation Plan

> For agentic workers: execute the tasks in order, with verification after each boundary.

**Goal:** Keep ChatGPT subscription conversation generation and move avatar rendering, motion and speech synthesis to this Mac without consuming Perxona credits.

**Architecture:** Bundle Three.js and @pixiv/three-vrm locally, render a redistributed official sample VRM, animate normalized bones and expressions locally. Generate WAV with macOS `say`; play and analyze that audio in the browser for lip movement. Preserve the existing Codex CLI bridge.

**Tech Stack:** Node.js, Three.js 0.180, @pixiv/three-vrm 3.5.5, esbuild, macOS say, browser Web Audio.

**Spec:** This document supersedes Perxona-specific requirements in the earlier prototype spec.

## Global Constraints

- Version stays 0.1.0. Only change projects/perxona-companion; preserve ignored .env and unrelated work.
- No remote renderer, CDN, Perxona request, paid API fallback or browser cloud speech synthesis/recognition.
- Chat generation retains the working ChatGPT-authenticated Codex CLI implementation and its UTF-8, input limit, timeout and sandbox protections.
- Preserve the Luma layout; communicate speech readiness and failures honestly; stop audio on demand including mobile layouts.
- Include local model source/license metadata; asset is VRM Public License 1.0, library code MIT.

## Task 1: Local speech endpoint

Files: lib/speech.mjs, server.mjs, test/speech.test.mjs, test/server.test.mjs.

- [x] Write and run failing tests for installed Japanese voice parsing, invalid text/voice rejection, safe process arguments, timeout and temporary-file cleanup.
- [x] Implement voice enumeration with `/usr/bin/say -v ?`; default to installed Kyoko Japanese voice.
- [x] Implement POST /api/speech accepting {text, voice}; pipe text to say stdin, produce WAVE/LEI16 in an isolated temporary directory, return validated WAV, clean directory on every exit. Bound input, concurrency and timeout.
- [x] Remove Perxona catalog/key/credit routes. Serve bundled local JS and VRM with safe path handling and a self-only runtime CSP.
- [x] Run meaningful endpoint tests, including inaccessible .env, origins and removed remote integrations.

## Task 2: Local avatar and playback

Files: public/avatar.js, public/app.js, public/index.html, public/style.css, public/assets/models/luma.vrm, package.json, package-lock.json.

- [x] Pin dependencies and add local build/start scripts; bundle JS without CDN imports.
- [x] Copy verified /tmp/luma-local-avatar.vrm into the app. Load it through GLTFLoader/VRMLoaderPlugin, frame upper body, lower neutral arms, animate blink/breath and nod/wave/bow with normalized bones.
- [x] Expose a small renderer API for speaking amplitude, gesture, update and disposal. Restore neutral pose/expression after playback.
- [x] Fetch locally generated WAV, play through Web Audio analyser, drive mouth from actual amplitude, release audio resources and restore controls after errors/stops.
- [x] Replace Perxona settings/credit copy with installed local voice, gesture and background controls; remove cloud speech fallback/input.
- [x] Run build/check/tests and verify avatar, speech, mouth motion, gesture, stop, settings and chat in the browser.

## Task 3: Documentation and delivery

Files: README.md, .env.example, .gitignore, THIRD_PARTY_NOTICES.md, docs/spec.md.

- [x] Document npm ci/start, Mac requirement, local assets/TTS, ChatGPT internet and subscription usage limits, and Flutter WebView portability limits.
- [x] Record official pixiv sample URL, embedded VRM metadata, VRM Public License and dependency licenses. Do not call the model CC0/MIT.
- [x] Record actual QA and known limitations without claiming human listening if it was not performed.
- [ ] Parent reviews implementation, verifies tests and browser, commits only this project excluding secrets, pushes an authorized branch and verifies remote hash.
