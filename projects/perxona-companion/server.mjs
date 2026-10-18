import { createServer as createHttpServer } from "node:http";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { CodexError, generateReply } from "./lib/codex.mjs";
import { DEFAULT_VOICE, SpeechError, listVoices, synthesizeSpeech } from "./lib/speech.mjs";

const ROOT = path.dirname(fileURLToPath(import.meta.url));
const MAX_BODY_BYTES = 96_000;
const CSP = "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; connect-src 'self' blob:; img-src 'self' data: blob:; media-src 'self' blob:; worker-src 'self' blob:; object-src 'none'; base-uri 'none'; frame-ancestors 'none'";
const STATIC_FILES = new Map([
  ["/", ["index.html", "text/html; charset=utf-8"]],
  ["/index.html", ["index.html", "text/html; charset=utf-8"]],
  ["/app.js", ["app.js", "text/javascript; charset=utf-8"]],
  ["/conversation.js", ["conversation.js", "text/javascript; charset=utf-8"]],
  ["/avatar.bundle.js", ["avatar.bundle.js", "text/javascript; charset=utf-8"]],
  ["/style.css", ["style.css", "text/css; charset=utf-8"]],
  ["/assets/models/luma.vrm", ["assets/models/luma.vrm", "model/gltf-binary"]],
]);

function baseHeaders(extra = {}) {
  return { "cache-control": "no-store", "x-content-type-options": "nosniff", "content-security-policy": CSP, ...extra };
}
function json(response, status, body) {
  response.writeHead(status, baseHeaders({ "content-type": "application/json; charset=utf-8" }));
  response.end(JSON.stringify(body));
}
function hostIsLocal(request) {
  const hostname = (request.headers.host || "").split(":")[0].replace(/^\[|\]$/g, "");
  return ["127.0.0.1", "localhost", "luma.localhost", "::1"].includes(hostname);
}
function originIsLocal(request) {
  if (!request.headers.origin) return true;
  try { const url = new URL(request.headers.origin); return url.protocol === "http:" && url.host === request.headers.host; }
  catch { return false; }
}
async function readJsonBody(request) {
  let size = 0; const chunks = [];
  for await (const chunk of request) {
    size += chunk.length;
    if (size > MAX_BODY_BYTES) throw Object.assign(new Error("リクエストが大きすぎます。"), { status: 413 });
    chunks.push(chunk);
  }
  try { return JSON.parse(Buffer.concat(chunks).toString("utf8")); }
  catch { throw Object.assign(new Error("JSONの形式が正しくありません。"), { status: 400 }); }
}

export function createServer(options = {}) {
  const env = options.env ?? process.env;
  const generateReplyImpl = options.generateReplyImpl ?? generateReply;
  const listVoicesImpl = options.listVoicesImpl ?? listVoices;
  const synthesizeSpeechImpl = options.synthesizeSpeechImpl ?? synthesizeSpeech;
  return createHttpServer(async (request, response) => {
    try {
      if (!hostIsLocal(request)) return json(response, 403, { error: "localhostからのみ利用できます。" });
      const url = new URL(request.url, "http://" + request.headers.host);
      if (url.pathname.includes("..")) return json(response, 400, { error: "不正なパスです。" });
      if (request.method === "GET" && url.pathname === "/api/config") {
        const voices = await listVoicesImpl();
        const japanese = voices.filter((voice) => voice.locale === "ja_JP");
        return json(response, 200, {
          codex: { model: env.CODEX_MODEL || "gpt-5.6-sol" },
          avatar: { modelUrl: "/assets/models/luma.vrm", gestures: ["auto", "nod", "wave", "bow"] },
          speech: { voices: japanese.length ? japanese : voices, defaultVoice: voices.some((v) => v.name === DEFAULT_VOICE) ? DEFAULT_VOICE : (japanese[0]?.name || voices[0]?.name || "") },
        });
      }
      if (request.method === "POST" && url.pathname === "/api/chat") {
        if (!originIsLocal(request)) return json(response, 403, { error: "許可されていないOriginです。" });
        const body = await readJsonBody(request);
        if (!body || typeof body !== "object" || !Array.isArray(body.messages) || body.messages.length === 0) return json(response, 400, { error: "メッセージを入力してください。" });
        return json(response, 200, await generateReplyImpl(body.messages, { model: env.CODEX_MODEL || "gpt-5.6-sol", env, codexPath: env.CODEX_BIN }));
      }
      if (request.method === "POST" && url.pathname === "/api/speech") {
        if (!originIsLocal(request)) return json(response, 403, { error: "許可されていないOriginです。" });
        const body = await readJsonBody(request);
        if (!body || typeof body !== "object") return json(response, 400, { error: "読み上げる文章を入力してください。" });
        const controller = new AbortController();
        const stopOnDisconnect = () => { if (!response.writableEnded) controller.abort(); };
        response.once("close", stopOnDisconnect);
        let wav;
        try { wav = await synthesizeSpeechImpl(body.text, { voice: body.voice, signal: controller.signal }); }
        finally { response.off("close", stopOnDisconnect); }
        response.writeHead(200, baseHeaders({ "content-type": "audio/wav", "content-length": wav.length }));
        return response.end(wav);
      }
      const staticFile = STATIC_FILES.get(url.pathname);
      if (request.method === "GET" && staticFile) {
        const [filename, contentType] = staticFile;
        const content = await readFile(path.join(ROOT, "public", filename));
        response.writeHead(200, baseHeaders({ "content-type": contentType, "cache-control": filename.endsWith(".vrm") ? "public, max-age=3600" : "no-cache" }));
        return response.end(content);
      }
      return json(response, 404, { error: "見つかりません。" });
    } catch (error) {
      const known = error instanceof CodexError || error instanceof SpeechError || Number.isInteger(error?.status);
      return json(response, known ? error.status : 500, { error: known ? error.message : "処理中にエラーが発生しました。", code: error.code });
    }
  });
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const port = Number.parseInt(process.env.PORT || "8788", 10);
  createServer().listen(port, "127.0.0.1", () => console.log("Luma is ready at http://127.0.0.1:" + port));
}
