# Snake / Dimension

Three.js で描画する、20 × 20 の 3D スネークゲームです。立体メッシュ、俯瞰カメラ、照明、影、奥行きを使いながら、キーボードと画面上のボタンの両方で遊べます。

ゲームロジックと操作仕様は、同じリポジトリにある先行実験 `../snake-game` を基準にしています。このプロジェクト内に必要なコードを複製しており、実行時の依存関係はありません。3D 描画には、主に 2D 描画を対象とする PixiJS ではなく Three.js を採用しました。

## 実行

```bash
npm install
npm run dev
```

表示されたローカル URL を WebGL 対応ブラウザで開きます。

## 操作

- 移動: 矢印キー、WASD、画面上の方向ボタン
- 開始・一時停止・再開: Space または画面上のボタン
- リスタート: R または画面上のボタン
- 盤面の「上」は画面奥、「下」は画面手前に対応

## 検証

```bash
npm test
npm run typecheck
npm run build
npm audit
```

実験条件と事前の受け入れ基準は [EXPERIMENT.md](./EXPERIMENT.md)、実測結果は [RESULTS.md](./RESULTS.md) に記録しています。
