import assert from "node:assert/strict";
import test from "node:test";
import { boundedHistory, restoreFailedDraft } from "../public/conversation.js";

test("会話履歴を12件・各4000文字・合計16000文字以内に保つ", () => {
  const history = Array.from({ length: 20 }, (_, index) => ({ role: index % 2 ? "assistant" : "user", content: String(index).repeat(5000) }));
  const bounded = boundedHistory(history);
  assert.ok(bounded.length <= 12);
  assert.ok(bounded.every((message) => message.content.length <= 4000));
  assert.ok(bounded.reduce((sum, message) => sum + message.content.length, 0) <= 16000);
  assert.equal(bounded.at(-1).content.startsWith("19"), true);
});

test("失敗した本文は新しい下書きを上書きしない", () => {
  assert.equal(restoreFailedDraft("", "失敗した本文"), "失敗した本文");
  assert.equal(restoreFailedDraft("次の下書き", "失敗した本文"), "次の下書き");
});
