# 3D Snake Game 実験記録

## 目的

AI モデルが、既存の小規模ブラウザゲームの操作性とルールを保ちつつ、実際の 3D ジオメトリ、カメラ、照明、影を備えた独立プロジェクトへ発展させられるか観察する。

## 実際のユーザープロンプト

以下は親タスクから共有された最新のユーザー発言の原文である。

`良い感じだね。今度はそれをPixiJSとか使って3D要素入れられる？`

## 実装担当への追加指示

以下は、この実験を実装する担当エージェントに渡された追加指示の原文である。

```text
Implement a SECOND independent experiment under /Users/yugo/ghq/github.com/yugo-ibuki/bitbit-de-but-be-dev/projects/model-performance/snake-game-3d. You own ONLY this new directory. You are not alone in the codebase; preserve others' edits, especially untracked .agents/skills/model-performance and the original projects/model-performance/snake-game. Do not modify the original. User's exact latest prompt: '良い感じだね。今度はそれをPixiJSとか使って3D要素入れられる？' Prior context: the original snake-game is a playable Vanilla TypeScript + Vite game with 20x20 board, keyboard arrows/WASD, on-screen direction controls, start/pause/resume/restart, score, food, wall/self collision, tests, and experiment records. User wants a 3D enhanced version. PixiJS official docs describe it as primarily 2D; parent design selects Three.js for true 3D geometry/camera/lighting. Applicable root AGENTS.md: subprojects independent, Japanese response, check AGENTS.local.md before work. Read .agents/skills/model-performance/SKILL.md fully and apply it. Model implementing this task: gpt-5.6-sol, reasoning medium. Record unknown conditions as unknown.
Design/acceptance criteria set before coding: create separate self-contained TypeScript/Vite project, reuse or copy existing pure game logic/tests without cross-project runtime dependency and credit the baseline in docs. Render actual WebGL 3D board with Three.js scene, mesh geometry for snake segments and food, angled orthographic camera, lighting/shadows/depth. Keep gameplay and responsive accessible HTML controls/state overlays. Cap device pixel ratio and resize renderer responsively; dispose resources where appropriate. Provide a visible message if WebGL setup fails (do not silently show blank board). Ensure board directions remain understandable from angle. Preserve all user controls/mechanics (arrows/WASD, visible touch buttons, start/pause/resume/restart, score, collisions, reversal guard, food placement). Make visually polished, but avoid unnecessary complexity and libraries. No version bumps on original. Include README, EXPERIMENT.md with exact user prompt and your handoff prompt verbatim (or clearly label summaries) and technical conditions, pre-defined observable criteria; RESULTS.md with actual build/test observations and unknowns. Tests should cover core game logic, not implementation-mirroring visual tests. Run npm install, test, typecheck/build, perhaps audit if relevant. Do not claim browser rendering without actual browser test. No commit, push, deploy. Parent will review and do independent browser QA on localhost different port from original 5173. Report files and exact verification.
```

## 生成条件

- モデル識別子: `gpt-5.6-sol`
- 推論設定: `medium`
- 実装日: 2026-09-23
- 所要時間: 不明
- モデル呼び出し回数: 不明
- 利用手段: Codex のファイル編集機能、シェルコマンド
- 技術構成: Vanilla TypeScript、Three.js 0.186.0、Vite 8.3.0、Vitest 5.0.1
- 実装環境: macOS。ブラウザとバージョンは未確認。
- 基準実装: `../snake-game` のゲームルールと純粋ロジック。実行時依存関係はない。

## 技術選定

ユーザー発言の「PixiJSとか」はライブラリ指定ではなく 3D 要素を求める例示と解釈した。立体ジオメトリ、カメラ、照明、影を直接扱える Three.js を選択した。

## 事前に定めた受け入れ基準

- 先行実験を変更せず、独立した Vite + Vanilla TypeScript プロジェクトとしてインストール、型検査、ビルドできる。
- Three.js の WebGL シーンに、立体の盤面、スネーク、食べ物を配置する。
- 斜め上から見下ろすカメラ、照明、影、奥行きによって 3D 表現を目視できる。
- 20 × 20 の盤面でスネークが一定間隔で移動する。
- 矢印キー、WASD、常時見える画面上の方向ボタンで操作できる。
- 開始、一時停止、再開、リスタートが操作でき、各操作にアクセシブルなボタン名がある。
- スコア、開始前・進行中・一時停止・ゲーム終了の状態が画面と支援技術へ伝わる。
- 壁または自分自身への衝突でゲームが終了し、明確な終了表示が出る。
- 進行方向と正反対の入力を無視し、食べ物はスネーク上に配置しない。
- 移動、捕食、衝突、停止、反転防止、食べ物配置、リスタートを純粋ロジックのテストで確認する。
- 盤面上方向の意味を視覚表示とアクセシブルな説明の両方で伝える。
- 描画の device pixel ratio を制限し、表示領域に応じてレンダラーとカメラを更新する。
- WebGL 初期化に失敗した場合、空白盤面ではなく説明を表示する。
- 描画ループ、レンダラー、監視処理、共有ジオメトリ、マテリアルを終了時に解放できる。
- スマートフォン幅でも盤面と操作が横にはみ出さない。
