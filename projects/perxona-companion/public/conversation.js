export function boundedHistory(messages, { maxTurns = 12, maxChars = 16_000, maxMessageChars = 4_000 } = {}) {
  const result = [];
  let total = 0;
  for (const message of messages.slice(-maxTurns).reverse()) {
    const content = String(message.content || "").slice(0, maxMessageChars);
    if (!content || total + content.length > maxChars) continue;
    result.push({ role: message.role, content });
    total += content.length;
  }
  return result.reverse();
}

export function restoreFailedDraft(currentDraft, failedText) {
  return currentDraft.trim() ? currentDraft : failedText;
}
