import assert from "node:assert/strict";
import http from "node:http";
import test from "node:test";
import { createServer } from "../server.mjs";

async function withServer(options, run) {
  const server = createServer(options);
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address();
  try { await run(`http://127.0.0.1:${port}`); }
  finally { await new Promise((resolve) => server.close(resolve)); }
}

function requestWithHost(baseUrl, pathname, host, origin) {
  const target = new URL(baseUrl);
  return new Promise((resolve, reject) => {
    const req = http.request({ hostname: target.hostname, port: target.port, path: pathname,
      headers: { host, ...(origin ? { origin } : {}) } }, (res) => {
      res.resume(); res.once("end", () => resolve({ status: res.statusCode, headers: res.headers }));
    });
    req.once("error", reject); req.end();
  });
}

test("configはローカルavatarとsay voiceだけを返す", async () => {
  await withServer({ env: {}, listVoicesImpl: async () => [{ name: "Kyoko (日本語（日本）)", locale: "ja_JP" }] }, async (base) => {
    const response = await fetch(`${base}/api/config`);
    const body = await response.json();
    assert.equal(body.avatar.modelUrl, "/assets/models/luma.vrm");
    assert.equal(body.speech.defaultVoice, "Kyoko (日本語（日本）)");
    assert.equal(JSON.stringify(body).includes("perxona"), false);
    assert.match(response.headers.get("content-security-policy"), /default-src 'self'/);
    assert.doesNotMatch(response.headers.get("content-security-policy"), /https:/);
  });
});

test("speechはsame-originだけ許可しWAVを返す", async () => {
  await withServer({ env: {}, listVoicesImpl: async () => [], synthesizeSpeechImpl: async (text, options) => {
    assert.equal(text, "こんにちは"); assert.equal(options.voice, "Kyoko (日本語（日本）)"); return Buffer.from("RIFFfakeWAVE");
  } }, async (base) => {
    const response = await fetch(`${base}/api/speech`, { method: "POST", headers: { "content-type": "application/json", origin: base }, body: JSON.stringify({ text: "こんにちは", voice: "Kyoko (日本語（日本）)" }) });
    assert.equal(response.status, 200);
    assert.equal(response.headers.get("content-type"), "audio/wav");
    assert.equal(Buffer.from(await response.arrayBuffer()).toString(), "RIFFfakeWAVE");
    const denied = await fetch(`${base}/api/speech`, { method: "POST", headers: { "content-type": "application/json", origin: "https://attacker.example" }, body: "{}" });
    assert.equal(denied.status, 403);
  });
});

test("null本文を400で拒否し旧Perxona APIは存在しない", async () => {
  await withServer({ env: {}, listVoicesImpl: async () => [] }, async (base) => {
    const response = await fetch(`${base}/api/speech`, { method: "POST", headers: { "content-type": "application/json", origin: base }, body: "null" });
    assert.equal(response.status, 400);
    for (const path of ["/api/connect-key", "/api/catalog/avatars", "/api/credit"]) assert.equal((await fetch(base + path)).status, 404);
  });
});

test("VRM assetを限定配信し外部Hostを拒否する", async () => {
  await withServer({ env: {}, listVoicesImpl: async () => [] }, async (base) => {
    const asset = await fetch(`${base}/assets/models/luma.vrm`);
    assert.equal(asset.status, 200);
    assert.equal(asset.headers.get("content-type"), "model/gltf-binary");
    assert.equal((await requestWithHost(base, "/api/config", "luma.localhost")).status, 200);
    assert.equal((await requestWithHost(base, "/api/config", "attacker.example")).status, 403);
  });
});

test("空の会話を400で拒否する", async () => {
  await withServer({ env: {}, listVoicesImpl: async () => [], generateReplyImpl: () => assert.fail() }, async (base) => {
    const response = await fetch(`${base}/api/chat`, { method: "POST", headers: { "content-type": "application/json", origin: base }, body: JSON.stringify({ messages: [] }) });
    assert.equal(response.status, 400);
  });
});
