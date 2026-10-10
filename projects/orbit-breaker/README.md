# ORBIT BREAKER

60秒間の軌道防衛に挑む、Phaser・TypeScript製の縦スクロールシューティングです。このディレクトリだけで依存関係、開発サーバー、テスト、オフラインビルドが完結します。

## 起動方法

```sh
npm ci
npm run dev
```

ターミナルに表示されたURLをブラウザで開き、「出撃する」を押してください。矢印キーまたはWASDで移動します。スマートフォンでは画面をドラッグして移動できます。レーザーは自動発射です。P・Esc・右下のボタンで一時停止できます。

## ゲーム仕様

- 制限時間60秒、シールド3個
- 撃破数に応じて最大LV15まで自動強化
- LV8で追尾ミサイル、LV15で5秒周期・幅180の貫通ビームを解放
- 2秒以内の連続撃破で最大5倍のコンボ倍率
- LV1〜3は高耐久編隊、LV4〜5は火力優位、LV6以降は回避路を残した強敵編隊
- キーボード、マウス、タッチ操作に対応

## オフライン版

```sh
npm run build
```

Phaser、ゲームコード、CSSを埋め込んだ `dist/index.html` が生成されます。このファイルはサーバーやネットワーク接続なしで直接開いて遊べます。

## 構成

- `index.html`: Vite開発用テンプレート
- `src/simulation.ts`: ゲーム状態、武器、敵、衝突、進行
- `src/main.ts`: Phaser描画、入力、DOM UI、効果音
- `src/style.css`: HUD、メニュー、レスポンシブ表示
- `src/simulation.test.ts`: シミュレーションの回帰テスト
- `scripts/build.mjs`: 単一HTMLの生成
- `scripts/smoke.mjs`: 生成HTMLの構文・外部参照・必須UI検証

## 検証

```sh
npm run check
```

型チェック、テスト、ビルド、オフラインHTMLのスモーク検証を順番に実行します。個別には `npm run typecheck`、`npm test`、`npm run build`、`npm run smoke` を使用できます。
