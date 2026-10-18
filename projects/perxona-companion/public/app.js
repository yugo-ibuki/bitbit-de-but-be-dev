import { createAvatar } from "./avatar.bundle.js";
import { boundedHistory, restoreFailedDraft } from "./conversation.js";

const $ = (selector) => document.querySelector(selector);
const elements = {
  host: $("#avatar-host"), placeholder: $("#stage-placeholder"), stageStatus: $("#stage-status"), diagnostic: $("#diagnostic-status"),
  connection: $("#connection-label"), pill: $("#connection-pill"), settings: $("#settings-dialog"), settingsButton: $("#settings-button"),
  voice: $("#voice-select"), gesture: $("#gesture-select"), background: $("#background-select"), motion: $("#play-motion-button"),
  stop: $("#interrupt-button"), model: $("#model-badge"), messages: $("#messages"), suggestions: $("#suggestions"),
  form: $("#chat-form"), input: $("#message-input"), send: $("#send-button"), error: $("#chat-error"),
};
const state = { config: null, avatar: null, history: [], pending: false, composing: false, audioContext: null, source: null, abort: null, playbackId: 0, analyserFrame: 0, amplitudePeak: 0 };
const SETTINGS_KEY = "luma.localSettings.v1";

function setStatus(text, kind = "") {
  elements.stageStatus.textContent = text; elements.connection.textContent = text;
  elements.pill.className = ("connection-pill " + kind).trim();
}
function diagnostic(phase) {
  elements.host.dataset.phase = phase;
  const labels = { loading: "3Dモデルを準備中", loaded: "準備できました", synthesizing: "音声を準備中", speaking: "読み上げ中", completed: "待機中", stopped: "停止しました", gesture: "動きを再生中", error: "エラーを確認してください" };
  elements.diagnostic.textContent = labels[phase] || "準備中";
  console.info("[Luma Local]", JSON.stringify({ phase, amplitudePeak: Number(state.amplitudePeak.toFixed(3)) }));
}
function friendly(error) { return error instanceof Error && error.message ? error.message : "処理に失敗しました。"; }
async function jsonRequest(url, options) {
  const response = await fetch(url, options); const data = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(data.error || "HTTP " + response.status); return data;
}
function restoreSettings() {
  try {
    const saved = JSON.parse(localStorage.getItem(SETTINGS_KEY) || "{}");
    for (const [element, value] of [[elements.voice, saved.voice], [elements.gesture, saved.gesture], [elements.background, saved.background]]) {
      if (value && [...element.options].some((option) => option.value === value)) element.value = value;
    }
  } catch {}
  applyBackground();
}
function saveSettings() {
  localStorage.setItem(SETTINGS_KEY, JSON.stringify({ voice: elements.voice.value, gesture: elements.gesture.value, background: elements.background.value }));
  applyBackground();
}
function applyBackground() { $("#stage").dataset.background = elements.background.value; }
async function ensureAudioContext() {
  state.audioContext ||= new AudioContext();
  if (state.audioContext.state === "suspended") await state.audioContext.resume();
  return state.audioContext;
}
function stopPlayback() {
  state.playbackId += 1; state.abort?.abort(); state.abort = null;
  try { state.source?.stop(); } catch {}
  state.source = null; cancelAnimationFrame(state.analyserFrame); state.avatar?.stop(); state.amplitudePeak = 0;
  elements.stop.disabled = true; setStatus(state.avatar ? "Lumaは準備できています" : "読み込み中", state.avatar ? "ready" : ""); diagnostic("stopped");
}
async function speak(text) {
  if (state.source || state.abort) stopPlayback();
  const playbackId = ++state.playbackId;
  const context = await ensureAudioContext();
  state.abort = new AbortController(); elements.stop.disabled = false; setStatus("音声を準備中…"); diagnostic("synthesizing");
  try {
    const response = await fetch("/api/speech", { method: "POST", headers: { "content-type": "application/json" }, signal: state.abort.signal, body: JSON.stringify({ text, voice: elements.voice.value }) });
    if (!response.ok) { const data = await response.json().catch(() => ({})); throw new Error(data.error || "音声を生成できませんでした。"); }
    if (playbackId !== state.playbackId) return;
    const buffer = await context.decodeAudioData(await response.arrayBuffer());
    if (playbackId !== state.playbackId) return;
    state.abort = null;
    const source = context.createBufferSource(), analyser = context.createAnalyser(), samples = new Uint8Array(512);
    analyser.fftSize = 1024; source.buffer = buffer; source.connect(analyser); analyser.connect(context.destination); state.source = source; state.amplitudePeak = 0;
    const gesture = elements.gesture.value === "auto" ? "nod" : elements.gesture.value; state.avatar?.playGesture(gesture);
    function track() {
      if (playbackId !== state.playbackId) return;
      analyser.getByteTimeDomainData(samples);
      let sum = 0; for (const sample of samples) { const value = (sample - 128) / 128; sum += value * value; }
      const amplitude = Math.min(1, Math.sqrt(sum / samples.length) * 5.5);
      state.amplitudePeak = Math.max(state.amplitudePeak, amplitude); state.avatar?.setAmplitude(amplitude);
      elements.host.dataset.amplitudePeak = state.amplitudePeak.toFixed(3); state.analyserFrame = requestAnimationFrame(track);
    }
    source.onended = () => {
      if (playbackId !== state.playbackId) return;
      cancelAnimationFrame(state.analyserFrame); state.avatar?.stop(); state.source = null; elements.stop.disabled = true;
      setStatus("Lumaは準備できています", "ready"); diagnostic("completed");
    };
    source.start(); track(); setStatus("話しています…", "ready"); diagnostic("speaking");
  } catch (error) {
    if (playbackId === state.playbackId) {
      state.abort = null; state.source = null; cancelAnimationFrame(state.analyserFrame); state.avatar?.stop(); elements.stop.disabled = true;
      setStatus(error.name === "AbortError" ? "発話を停止しました" : "音声を再生できません", error.name === "AbortError" ? "ready" : "error");
      diagnostic(error.name === "AbortError" ? "stopped" : "error");
    }
    throw error;
  }
}
function appendMessage(role, text) {
  const article = document.createElement("article"); article.className = "message " + role;
  const avatar = document.createElement("div"); avatar.className = "avatar-mini"; avatar.textContent = role === "assistant" ? "L" : "You";
  const body = document.createElement("div"), speaker = document.createElement("span"), paragraph = document.createElement("p");
  speaker.className = "speaker"; speaker.textContent = role === "assistant" ? "Luma" : "あなた"; paragraph.textContent = text;
  body.append(speaker, paragraph); article.append(avatar, body); elements.messages.append(article); elements.messages.scrollTop = elements.messages.scrollHeight;
}
async function sendMessage(text) {
  if (state.pending) return;
  if (state.source || state.abort) stopPlayback();
  state.pending = true; elements.send.disabled = true; elements.error.hidden = true; elements.suggestions.hidden = true;
  appendMessage("user", text); state.history = boundedHistory([...state.history, { role: "user", content: text }]);
  try {
    const result = await jsonRequest("/api/chat", { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ messages: state.history }) });
    appendMessage("assistant", result.text); state.history = boundedHistory([...state.history, { role: "assistant", content: result.text }]);
    try {
      await speak(result.text);
    } catch (error) {
      if (error.name !== "AbortError") {
        elements.error.textContent = "返答は届きましたが、音声を再生できませんでした: " + friendly(error);
        elements.error.hidden = false;
      }
    }
  } catch (error) {
    state.history = state.history.filter((message, index, messages) => !(index === messages.length - 1 && message.role === "user" && message.content === text));
    elements.input.value = restoreFailedDraft(elements.input.value, text);
    elements.error.textContent = "会話を送信できませんでした: " + friendly(error); elements.error.hidden = false; diagnostic("error");
  } finally { state.pending = false; elements.send.disabled = false; elements.input.focus(); }
}
async function bootstrap() {
  try {
    state.config = await jsonRequest("/api/config"); elements.model.textContent = "Codex · " + state.config.codex.model;
    elements.voice.replaceChildren(...state.config.speech.voices.map((voice) => new Option(voice.name, voice.name)));
    elements.voice.value = state.config.speech.defaultVoice; restoreSettings();
    state.avatar = await createAvatar({
      container: elements.host,
      modelUrl: state.config.avatar.modelUrl,
      onStatus: diagnostic,
      onGestureComplete() {
        if (elements.host.dataset.phase === "gesture") {
          setStatus("Lumaは準備できています", "ready");
          diagnostic("completed");
        }
      },
    });
    elements.placeholder.hidden = true; setStatus("Lumaは準備できています", "ready"); diagnostic("loaded");
  } catch (error) {
    setStatus("準備エラー", "error"); $("#stage-message").textContent = "Lumaを読み込めませんでした"; $("#stage-detail").textContent = friendly(error); diagnostic("error");
  }
}
elements.form.addEventListener("submit", async (event) => {
  event.preventDefault(); if (state.composing || state.pending) return;
  await ensureAudioContext(); if (state.pending) return;
  const text = elements.input.value.trim(); if (!text) return;
  elements.input.value = ""; await sendMessage(text);
});
elements.input.addEventListener("compositionstart", () => { state.composing = true; });
elements.input.addEventListener("compositionend", () => { state.composing = false; });
elements.input.addEventListener("keydown", (event) => { if (event.key === "Enter" && !event.shiftKey && !event.isComposing) { event.preventDefault(); elements.form.requestSubmit(); } });
elements.stop.addEventListener("click", stopPlayback);
elements.settingsButton.addEventListener("click", () => elements.settings.showModal());
for (const element of [elements.voice, elements.gesture, elements.background]) element.addEventListener("change", saveSettings);
elements.motion.addEventListener("click", async () => { await ensureAudioContext(); state.avatar?.playGesture(elements.gesture.value === "auto" ? "wave" : elements.gesture.value); diagnostic("gesture"); });
for (const button of elements.suggestions.querySelectorAll("button")) button.addEventListener("click", async () => { await ensureAudioContext(); elements.input.value = button.textContent; elements.form.requestSubmit(); });
bootstrap();
