import { spawn } from "node:child_process";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

export const DEFAULT_VOICE = "Kyoko (日本語（日本）)";
const SAY = "/usr/bin/say";
const MAX_TEXT_LENGTH = 2000;
let active = false;

export class SpeechError extends Error {
  constructor(message, status = 500, code = "SPEECH_ERROR") {
    super(message); this.name = "SpeechError"; this.status = status; this.code = code;
  }
}

export function parseSayVoices(output) {
  const seen = new Set();
  return output.split(/\r?\n/).flatMap((line) => {
    const match = line.match(/^(.+?)\s+([a-z]{2}_[A-Z]{2})\s+#/);
    if (!match) return [];
    const voice = { name: match[1].trim(), locale: match[2] };
    const key = voice.name + "\n" + voice.locale;
    if (seen.has(key)) return [];
    seen.add(key);
    return [voice];
  });
}

function run(command, args, { spawnImpl = spawn, input = "", timeoutMs = 15_000, signal } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawnImpl(command, args, { shell: false, stdio: ["pipe", "pipe", "pipe"] });
    const stdout = [];
    let settled = false;
    const finish = (callback) => {
      if (settled) return; settled = true; clearTimeout(timer); signal?.removeEventListener("abort", abort); callback();
    };
    const abort = () => { child.kill?.("SIGKILL"); finish(() => reject(new SpeechError("音声生成を停止しました。", 499, "SPEECH_ABORTED"))); };
    const timer = setTimeout(() => { child.kill?.("SIGKILL"); finish(() => reject(new SpeechError("音声生成が時間切れになりました。", 504, "SPEECH_TIMEOUT"))); }, timeoutMs);
    signal?.addEventListener("abort", abort, { once: true });
    if (signal?.aborted) abort();
    child.stdout?.on("data", (chunk) => stdout.push(Buffer.from(chunk)));
    child.stderr?.on("data", () => {});
    child.once("error", (error) => finish(() => reject(new SpeechError("sayを起動できません: " + error.message, 503))));
    child.once("close", (code) => {
      finish(() => code === 0 ? resolve(Buffer.concat(stdout)) : reject(new SpeechError("sayが終了しました (" + code + ")", 500, "SAY_FAILED")));
    });
    child.stdin?.on?.("error", () => {});
    child.stdin?.end(input);
  });
}

export async function listVoices(options = {}) {
  const output = await run(SAY, ["-v", "?"], options);
  return parseSayVoices(output.toString("utf8"));
}

export async function synthesizeSpeech(text, options = {}) {
  if (typeof text !== "string" || !text.trim()) throw new SpeechError("読み上げる文章を入力してください。", 400);
  if (text.length > MAX_TEXT_LENGTH) throw new SpeechError("読み上げる文章が長すぎます。", 400);
  if (active) throw new SpeechError("音声を生成中です。少し待ってください。", 429, "SPEECH_BUSY");
  active = true;
  let directory;
  try {
    const voice = options.voice || DEFAULT_VOICE;
    const voices = options.voices != null ? await options.voices : await listVoices(options);
    if (!voices.some((item) => item.name === voice)) throw new SpeechError("選択したmacOS音声が見つかりません。", 400, "VOICE_NOT_FOUND");
    directory = await mkdtemp(path.join(tmpdir(), "luma-speech-"));
    const output = path.join(directory, "speech.wav");
    await run(SAY, ["-v", voice, "-o", output, "--file-format=WAVE", "--data-format=LEI16", "-f", "-"], { ...options, input: text });
    const wav = await readFile(output);
    if (wav.length < 12 || wav.subarray(0, 4).toString() !== "RIFF") throw new SpeechError("音声ファイルを生成できませんでした。");
    return wav;
  } finally {
    active = false;
    if (directory) await rm(directory, { recursive: true, force: true });
  }
}
