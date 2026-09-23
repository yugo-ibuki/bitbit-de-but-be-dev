# Snake Game 実験記録

## 目的

AI モデルが、指定された小規模ブラウザゲームを、純粋なゲームロジック、操作可能な UI、検証記録を含む独立プロジェクトとして実装できるか観察する。

## 実際のユーザープロンプト

以下は親タスクから共有されたユーザー発言の原文である。

1. `その中で色々ゲームとかを作って、その精度を見たい`
2. `tsでいいよ`
3. `良い感じです。一つプロジェクトを作って欲しいです。`

## 実装担当への追加指示

以下は、この実験を実装する担当エージェントに渡された追加指示の原文である。

> Implement the FIRST actual experiment/game project in /Users/yugo/ghq/github.com/yugo-ibuki/bitbit-de-but-be-dev/projects/model-performance/snake-game. You own ONLY this new directory. You are not alone in the codebase; preserve all other edits (especially untracked .agents/skills/model-performance/SKILL.md), do not revert others, and accommodate changes. User context verbatim: '良い感じです。一つプロジェクトを作って欲しいです。' Prior user clarified: 'その中で色々ゲームとかを作って、その精度を見たい'; they said TypeScript is okay. Game choice is parent assumption after optional question with no answer yet: Snake. If user changes genre, parent will steer. Repository root AGENTS.md: each project independent; Japanese final responses; check AGENTS.local.md in root and governing dirs (none found by parent, recheck). Read and apply repo skill .agents/skills/model-performance/SKILL.md completely. Model used to implement this game: gpt-5.6-sol, reasoning effort medium; record only known conditions, unknowns as unknown. Do not create benchmark harness or modify skill. Design/acceptance criteria BEFORE coding: independent vanilla TypeScript + Vite browser Snake in the new directory, polished responsive interface, 20x20 board, keyboard arrows/WASD and visible touch/on-screen controls, start/pause/restart, score, food, wall/self collision, no reversal exploit, food never on snake, clear end state. Include accessible buttons and status. Keep architecture small: pure game state logic separable from DOM, a few meaningful tests (movement/food/collision/restart), avoid unnecessary features/deps. Record a prompt log containing the actual user prompts and this implementation handoff (verbatim or clearly identify if summarized; do not falsely claim raw if summarized), implementation conditions, intended acceptance criteria before judging, and an experiment results file that distinguishes planned checks from observed tests. Run npm install, test, typecheck/build. Capture actual outcomes, not imagined browser QA. Do NOT commit/push or deploy. Keep this self-contained. If browser QA can be done, do it; otherwise tell parent what remains. Give me files, commands/results, and limitations. I will review and perform independent UI QA.

## 生成条件

- モデル識別子: `gpt-5.6-sol`
- 推論設定: `medium`
- 実装日: 2026-09-23
- 所要時間: 不明
- モデル呼び出し回数: 不明
- 利用手段: Codex のファイル編集機能、シェルコマンド
- 技術構成: Vanilla TypeScript、Vite、Vitest
- ブラウザと OS: 実装環境は macOS。画面確認には Codex In-app Browser を使用した。ブラウザの詳細バージョンは不明。

## 事前に定めた受け入れ基準

- 独立した Vite + Vanilla TypeScript プロジェクトとしてインストール、型検査、ビルドできる。
- 20 × 20 の盤面でスネークが一定間隔で移動する。
- 矢印キー、WASD、常時見える画面上の方向ボタンで操作できる。
- 開始、一時停止、再開、リスタートが操作でき、各操作にアクセシブルなボタン名がある。
- スコア、開始前・進行中・一時停止・ゲーム終了の状態が画面と支援技術へ伝わる。
- 壁または自分自身への衝突でゲームが終了し、明確な終了表示が出る。
- 進行方向と正反対の入力を無視し、即時反転による自己衝突を防ぐ。
- 食べ物はスネーク上に配置されず、食べると得点と長さが増える。
- 移動、捕食、壁・自己衝突、反転防止、リスタートを純粋なロジックのテストで確認する。
- レスポンシブな画面としてスマートフォン幅でも盤面と操作が収まる。
