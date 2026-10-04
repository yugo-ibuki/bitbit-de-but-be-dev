# Ocean Study — NAGI adaptation

参照映像の作者 noxellab が公開した [`noxellab/nagi-ocean-sim`](https://github.com/noxellab/nagi-ocean-sim) を、このリポジトリ内で動かせる独立プロジェクトとして移植したものです。描画実装を独自制作したものとして扱わず、上流の設計とライセンスを引き継いでいます。

- 上流コミット: [`16c552b1eda48249b23d633617d67127fe7755c1`](https://github.com/noxellab/nagi-ocean-sim/commit/16c552b1eda48249b23d633617d67127fe7755c1)
- 上流デモ: <https://noxelwork.com/nagi-ocean/>
- コードと泡画像: MIT License
- 空画像と放射輝度データ: CC0-1.0
- クジラモデル: Bohdan Lvov作「Blue Whale - Textured」、CC BY 4.0

ライセンス全文は [`LICENSE`](./LICENSE)、Three.js と空素材の通知は [`THIRD_PARTY_NOTICES.txt`](./THIRD_PARTY_NOTICES.txt) を参照してください。

## 表現

Three.js / WebGL 2 で、3 帯域のスペクトル波、波頭の泡、海底の屈折、HDR 環境光を描画します。GPU の浮動小数点レンダーターゲットが利用できない場合は、軽量な波の描画へ切り替わります。実行時の外部通信はありません。

## 起動

Node.js 24.15 以降の 24.x、または Node.js 26 以降を使用します。

```bash
npm ci
npm run dev
```

表示されたローカル URL を WebGL 2 対応ブラウザで開いてください。

## 操作

- ドラッグ: 視点移動
- スクロール / ピンチ: カメラの高さ
- `Space`: 再生・停止
- `H`: 操作パネルの表示・非表示
- 操作パネル: 情景、波高、風、太陽、描画品質の変更

初期表示では海を広く見せるため設定パネルを閉じています。画面右上の「設定」または `H` で開けます。描画品質はデスクトップ幅で「高精細」、700px 以下で「標準」から開始し、画面下部の品質ボタンで切り替えられます。

## この移植版で追加した表現

初期情景を波高 1.5・風 1.0 の「雨のあと」とし、テクスチャとスケルトンを持つ全長約24mのクジラを沖合に追加しています。Bohdan Lvov作「Blue Whale - Textured」の色、法線、ORMテクスチャとSwimmingアニメーションを使用しています。クジラは水上へ直接描画せず、波面近くの屈折する海中場面だけに置いているため、波の薄いところで輪郭と肌が見えます。胴体の中央・頭側・尾側・左右で同じGPU波面を採取し、その波高から上下、前後傾斜、左右傾斜を求め、質量と減衰を持つ状態として遅れて追従させています。泳ぐ速さには推進と惰性の緩急を付け、尾のアニメーション位相と移動量を同期しています。通過位置に沿う航跡は既存の波頭の泡へ弱く混ぜています。約55秒周期で沖を斜めに横切り、周期の境界では画面外でフェードします。「波間の影」で個別に非表示へ切り替えられ、海の停止中は泳ぎ、浮力姿勢、航跡も停止します。

モデルの作者・配布元・ライセンスは [`public/BLUE_WHALE_ATTRIBUTION.txt`](./public/BLUE_WHALE_ATTRIBUTION.txt) に記載しています。モデル読み込みに失敗した場合は低品質な代替形状を表示せず、海の描画だけを継続します。

## 検証

```bash
npm test
npm run build
```

`npm test` は数値処理、操作、リソース解放、読み込み失敗を確認します。GPU 描画はブラウザで別途確認します。

## 主な構成

- `src/ocean.js`: 描画、泡、操作
- `src/spectral-ocean.js`: 256² × 3 帯域の FFT 波
- `src/refraction.js`, `src/seabed.js`: 屈折と海底
- `src/creature-shadow.js`: クジラの読み込み、材質、泳ぎ、周期運動
- `src/creature-buoyancy.js`: 波面の複数点採取と慣性を持つ浮力姿勢
- `src/whale-breath.js`: 波面判定に連動する呼気と飛沫
- `src/photo-sky.js`, `src/atmosphere.js`: 空と環境光
- `public/`: 泡、空、放射輝度データ、クジラGLBと帰属表示
