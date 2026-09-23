# LAST LIGHT 実験記録

## 目的

AI モデルが PixiJS を使い、短時間で遊べる 2D ホラーゲームを、雰囲気・操作性・明確なゲームルール・検証可能なロジックを含む独立プロジェクトとして実装できるか観察する。

## 実際のユーザープロンプト

以下は親タスクから共有されたユーザー発言の原文である。

`ホラーゲームっぽいものって作れます？`

`作ってみてください`

## 操作性改善で受けたユーザーフィードバック

以下は親タスクから共有されたユーザー発言の原文である。

`操作感が難しいな`

`例えば、マウスを右手で触るのに、矢印で移動ってどっちも右手を使うじゃん？`

## 操作性改善で事前に定めた受け入れ基準

- 矢印キーを短く押した直後に、次の描画フレームを待たずプレイヤーが衝突判定付きで目に見えて移動し、懐中電灯も同じ方向を向く。
- 矢印キーを長押しすると、短押しの初動に続いて既存の連続移動が続き、懐中電灯は移動方向を向く。
- WASD を短く押すとプレイヤーが移動し、ゲームキャンバス上でマウスを動かした後は、WASD で移動中も懐中電灯をポインター方向へ独立して向けられる。
- モバイルの方向ボタンを短く押すと即時に移動して同じ方向を照らし、長押しすると連続移動する。
- キーの自動リピートや押下済み方向の重複イベントでは、短押し用の即時移動を繰り返さない。
- ゲーム開始・リスタート時は自動照準へ戻り、開始前オーバーレイ上のポインター移動では手動照準を有効にしない。
- ゲームループ、勝利、敗北、一時停止、リスタートの既存動作を維持する。
- 画面と README の操作説明に `WASD + マウス`、`矢印キーだけ（ライト自動）`、モバイル方向ボタンの役割を分けて示し、矢印キーとマウスの同時使用が必要とは読めないようにする。
- 純粋な方向・照準・入力判定ロジックを意味のあるテストで確認し、`npm test`、`npm run typecheck`、`npm run build` に成功する。
- 実ブラウザでの短押し・長押し・独立照準・モバイル操作・勝敗・リスタートは、親タスクが実操作で確認し、静的テストやビルド成功と区別して記録する。

## 実装担当への追加指示

以下は、この実験を実装する担当エージェントに渡された追加指示の原文である。

```text
Implement a NEW independent PixiJS 2D horror game experiment at /Users/yugo/ghq/github.com/yugo-ibuki/bitbit-de-but-be-dev/projects/model-performance/last-light. You own ONLY that new directory and implementation/docs there. You are not alone in the codebase; preserve all other changes and untracked files, do not revert others, and accommodate changes. Do not commit/push/deploy. User exact recent prompts (record verbatim in EXPERIMENT.md): `ホラーゲームっぽいものって作れます？` then `作ってみてください`. Earlier discussion: user asked what PixiJS can do; I proposed a short 2D exploration horror, flashlight, dark corridors, find a key/exit, as separate experiment. User accepted by asking to make it. Repository root AGENTS.md: independent projects, Japanese response, check AGENTS.local.md root/governing directories (parent found none; recheck), read and apply .agents/skills/model-performance/SKILL.md. Skill requires original prompts and exact implementation handoff recorded, known model/config (this implementation is gpt-5.6-sol, reasoning medium; unknowns explicitly unknown), predeclared observable criteria, actual observed checks and unknowns. Create EXPERIMENT.md with criteria before implementation, and RESULTS.md after checks. Record this handoff verbatim or link/provide exact text, without pretending a summary is verbatim. Design decided by parent: title e.g. LAST LIGHT, top-down dark ward or facility; collect 3 fuses then reach exit while a pursuing shadow threatens player. Focus on atmospheric, polished and playable rather than huge scope. Use PixiJS v8 + TypeScript + Vite, official v8 API (Application async init, Graphics rect/circle etc.). Independent package and lockfile. No external art assets required; vector scene can be polished. Core observable acceptance criteria: (1) npm install/typecheck/build succeed; (2) PixiJS canvas visibly renders atmospheric map, player, pickups, pursuer and flashlight visibility effect; (3) keyboard WASD/arrows movement and pointer aim; mobile visible touch controls if feasible; (4) 3 pickups unlock exit, proximity to pursuer loses, reaching unlocked exit wins, restart works; (5) obstacles block player and pursuer sensibly, enemy does not get stuck; (6) clear instructions, objective/progress, win/lose UI; (7) responsive desktop/mobile; (8) meaningful pure game logic tests for collection, collisions, victory, lose/pathfinding if implemented. Keep app self-contained and avoid unnecessary dependencies. UX: allow player to start easily, no hidden setup; useful audio optional only if robust. Include README. Verify commands and report exactly. Parent will inspect and independently browser QA. Official docs verified: https://pixijs.com/8.x/guides/components/application , https://pixijs.com/8.x/guides/components/scene-objects/graphics , https://pixijs.com/8.x/guides/components/scene-objects (masks), https://pixijs.com/8.x/guides/components/events . If requirements conflict, ask parent promptly. Return files, commands/results, limitations.
```

## 生成条件

- モデル識別子: `gpt-5.6-sol`
- 推論設定: `medium`
- 実装日: 2026-09-23
- 所要時間: 不明
- モデル呼び出し回数: 不明
- 利用手段: Codex のファイル編集機能、シェルコマンド
- 技術構成: Vanilla TypeScript、PixiJS 8、Vite、Vitest
- 実装環境: macOS。ブラウザとバージョンは未確認。

## 事前に定めた受け入れ基準

- 独立した Vite + Vanilla TypeScript + PixiJS v8 プロジェクトとして依存関係を解決し、型検査、テスト、本番ビルドに成功する。
- PixiJS のキャンバスに、荒廃した施設のマップ、プレイヤー、3個のヒューズ、追跡者、出口を表示する。
- プレイヤーの周囲とポインター方向だけが見える懐中電灯表現、ちらつき、画面効果によってホラーらしい視覚表現を作る。
- WASD または矢印キーで移動し、ポインターで懐中電灯の方向を変えられる。
- スマートフォンでも画面上の方向ボタンで移動し、ゲームを開始・再開できる。
- 壁や設備がプレイヤーを遮り、通り抜けられない。
- 追跡者が通路を経路探索してプレイヤーを追い、障害物に恒常的に引っ掛からない。
- 3個のヒューズへ近づくと回収され、すべて回収すると出口が解錠される。
- 解錠後の出口へ到達すると勝利し、追跡者へ接触すると敗北する。
- 勝敗後にリスタートすると初期状態から再開できる。
- 操作説明、目的、回収進捗、開始・勝利・敗北状態を明確に表示する。
- デスクトップとスマートフォン幅で、キャンバスと操作 UI が横にはみ出さない。
- 衝突、回収、解錠、勝利、敗北、経路探索を純粋ロジックのテストで確認する。
- 実ブラウザで未確認の見た目や操作は、静的確認やビルド成功と区別して記録する。
