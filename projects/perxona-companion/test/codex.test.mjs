import assert from "node:assert/strict";
import { chmod, mkdtemp, writeFile } from "node:fs/promises";
import { EventEmitter } from "node:events";
import { tmpdir } from "node:os";
import path from "node:path";
import { PassThrough } from "node:stream";
import test from "node:test";

import { CodexError, generateReply } from "../lib/codex.mjs";

async function fakeCodex(source) {
  const directory = await mkdtemp(path.join(tmpdir(), "fake-codex-"));
  const executable = path.join(directory, "codex");
  await writeFile(executable, `#!/usr/bin/env node\n${source}\n`);
  await chmod(executable, 0o755);
  return executable;
}

test("ChatGPTログインではないCodex認証を拒否する", async () => {
  const codexPath = await fakeCodex(`
    if (process.argv.slice(2).join(" ") === "login status") {
      console.log("Logged in using an API key");
      process.exit(0);
    }
  `);

  await assert.rejects(
    generateReply([{ role: "user", content: "こんにちは" }], { codexPath }),
    (error) => error instanceof CodexError && error.code === "CHATGPT_LOGIN_REQUIRED",
  );
});

test("agent_messageとturn.completedが揃った実行結果だけを返す", async () => {
  const codexPath = await fakeCodex(`
    if (process.argv.slice(2).join(" ") === "login status") {
      console.log("Logged in using ChatGPT");
    } else {
      process.stdin.resume();
      process.stdin.on("end", () => {
        console.log(JSON.stringify({type:"item.completed",item:{type:"agent_message",text:"やあ、今日はどうしたの？"}}));
        console.log(JSON.stringify({type:"turn.completed",usage:{input_tokens:10,output_tokens:8}}));
      });
    }
  `);

  const reply = await generateReply(
    [
      { role: "user", content: "こんにちは" },
      { role: "assistant", content: "こんにちは。" },
      { role: "user", content: "今日はどうしよう？" },
    ],
    { codexPath, model: "test-model" },
  );

  assert.equal(reply.text, "やあ、今日はどうしたの？");
  assert.deepEqual(reply.usage, { input_tokens: 10, output_tokens: 8 });
});

test("turn.completedのない部分出力を成功扱いしない", async () => {
  const codexPath = await fakeCodex(`
    if (process.argv.slice(2).join(" ") === "login status") {
      console.log("Logged in using ChatGPT");
    } else {
      process.stdin.resume();
      process.stdin.on("end", () => {
        console.log(JSON.stringify({type:"item.completed",item:{type:"agent_message",text:"途中の返答"}}));
      });
    }
  `);

  await assert.rejects(
    generateReply([{ role: "user", content: "こんにちは" }], { codexPath }),
    (error) => error instanceof CodexError && error.code === "INCOMPLETE_TURN",
  );
});

test("stdoutのUTF-8文字がchunk境界で分割されても日本語を壊さない", async () => {
  let invocation = 0;
  const spawnImpl = () => {
    const child = new EventEmitter();
    child.stdout = new PassThrough();
    child.stderr = new PassThrough();
    child.stdin = new PassThrough();
    child.kill = () => {};
    const current = invocation++;
    child.stdin.on("finish", () => {
      queueMicrotask(() => {
        if (current === 0) {
          child.stdout.end("Logged in using ChatGPT\n");
        } else {
          const output = Buffer.from([
            JSON.stringify({ type: "item.completed", item: { type: "agent_message", text: "こんにちは" } }),
            JSON.stringify({ type: "turn.completed", usage: {} }),
            "",
          ].join("\n"));
          const characterStart = output.indexOf(Buffer.from("こ"));
          child.stdout.write(output.subarray(0, characterStart + 1));
          child.stdout.end(output.subarray(characterStart + 1));
        }
        child.stderr.end();
        child.emit("close", 0, null);
      });
    });
    return child;
  };

  const reply = await generateReply(
    [{ role: "user", content: "挨拶して" }],
    { codexPath: "/fake/codex", spawnImpl },
  );

  assert.equal(reply.text, "こんにちは");
});
