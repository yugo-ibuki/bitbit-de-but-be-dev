import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { readdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import test from "node:test";
import { parseSayVoices, synthesizeSpeech } from "../lib/speech.mjs";

test("say voice一覧から名前とlocaleを読む", () => {
  assert.deepEqual(parseSayVoices("Kyoko (日本語（日本）) ja_JP    # こんにちは\nKyoko (日本語（日本）) ja_JP # duplicate\nSamantha            en_US    # Hello"), [
    { name: "Kyoko (日本語（日本）)", locale: "ja_JP" }, { name: "Samantha", locale: "en_US" },
  ]);
});

test("speech本文を引数に含めずstdinへ渡しWAVを返す", async () => {
  let call;
  const spawnImpl = (command, args, options) => {
    call = { command, args, options, stdin: "" };
    const child = new EventEmitter(); child.stdin = { end(value) { call.stdin = value; } };
    child.kill = () => {};
    queueMicrotask(async () => { const { writeFile } = await import("node:fs/promises"); await writeFile(args[args.indexOf("-o") + 1], Buffer.from("RIFFfakeWAVE")); child.emit("close", 0, null); });
    return child;
  };
  const result = await synthesizeSpeech("安全な日本語", { voice: "Kyoko (日本語（日本）)", voices: [{ name: "Kyoko (日本語（日本）)", locale: "ja_JP" }], spawnImpl });
  assert.equal(result.toString(), "RIFFfakeWAVE");
  assert.equal(call.command, "/usr/bin/say");
  assert.equal(call.args.includes("安全な日本語"), false);
  assert.equal(call.args.includes("-f"), true);
  assert.equal(call.stdin, "安全な日本語");
  assert.equal(call.options.shell, false);
});

test("未導入voiceと長すぎる本文を拒否する", async () => {
  await assert.rejects(() => synthesizeSpeech("hello", { voice: "Missing", voices: [] }), /音声/);
  await assert.rejects(() => synthesizeSpeech("あ".repeat(2001), { voice: "Kyoko", voices: [{ name: "Kyoko" }] }), /長すぎ/);
});

test("voice確認待ちの間も二重生成を拒否し、失敗後はlockを解放する", async () => {
  let release;
  const voices = new Promise((resolve) => { release = resolve; });
  const first = synthesizeSpeech("一件目", { voice: "Missing", voices });
  await assert.rejects(() => synthesizeSpeech("二件目", { voice: "Missing", voices: [] }), /生成中/);
  release([]);
  await assert.rejects(() => first, /音声/);
  await assert.rejects(() => synthesizeSpeech("三件目", { voice: "Missing", voices: [] }), /見つかりません/);
});

test("say timeout後にlockを解放する", async () => {
  const before = new Set((await readdir(tmpdir())).filter((name) => name.startsWith("luma-speech-")));
  const spawnImpl = () => {
    const child = new EventEmitter(); child.stdin = { end() {}, on() {} }; child.kill = () => {};
    return child;
  };
  await assert.rejects(() => synthesizeSpeech("timeout", { voice: "Kyoko", voices: [{ name: "Kyoko" }], spawnImpl, timeoutMs: 5 }), /時間切れ/);
  const after = (await readdir(tmpdir())).filter((name) => name.startsWith("luma-speech-") && !before.has(name));
  assert.deepEqual(after, []);
  await assert.rejects(() => synthesizeSpeech("next", { voice: "Missing", voices: [] }), /見つかりません/);
});

test("AbortSignalでsayを停止しlockと一時directoryを解放する", async () => {
  const controller = new AbortController();
  const spawnImpl = () => {
    const child = new EventEmitter(); child.stdin = { end() {}, on() {} };
    child.kill = () => queueMicrotask(() => child.emit("close", null, "SIGKILL"));
    return child;
  };
  const pending = synthesizeSpeech("stop", { voice: "Kyoko", voices: [{ name: "Kyoko" }], spawnImpl, signal: controller.signal });
  controller.abort();
  await assert.rejects(() => pending, /停止/);
  await assert.rejects(() => synthesizeSpeech("next", { voice: "Missing", voices: [] }), /見つかりません/);
});
