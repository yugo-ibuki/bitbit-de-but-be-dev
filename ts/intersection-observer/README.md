# Intersection Observer Playground

TypeScript で実装された Intersection Observer API のプレイグラウンドです。要素の表示/非表示を監視し、視覚的フィードバックを提供します。

## ファイル構成

```
intersection-observer/
├── index.html          # メインHTMLファイル
├── styles.css          # スタイルシート
├── main.ts            # TypeScript実装
├── dist/
│   ├── main.js        # コンパイル済みJavaScript
│   └── main.js.map    # ソースマップ
├── package.json       # プロジェクト設定
└── tsconfig.json      # TypeScript設定
```

## 機能

### 1. 基本的な要素監視
- スクロール時に要素が画面内に入ると背景色が緑色に変化
- 複数の閾値（0, 0.25, 0.5, 0.75, 1.0）で監視
- 交差比率に応じた透明度の動的変更

### 2. フェードインアニメーション
- `.fade-in` クラスを持つ要素は表示時にフェードインアニメーション
- `opacity` と `transform` を使用したスムーズな表示

### 3. 遅延画像読み込み
- `.lazy-load` クラスを持つ要素内の画像を表示時に読み込み
- `data-src` 属性で画像URLを指定
- 読み込み失敗時のエラーハンドリング

### 4. リアルタイムステータス表示
- 画面右側のパネルで各要素の監視状況をリアルタイム表示
- 表示状態をインジケーターで視覚化

### 5. カスタムイベント
- 要素が表示された際に `elementVisible` カスタムイベントを発火
- イベントの詳細情報（要素、交差比率、ID）を含む

## クラス構造

### `IntersectionObserverPlayground`

メインクラス。Intersection Observer の設定と管理を行います。

#### プロパティ
- `observer`: IntersectionObserver インスタンス
- `observedElements`: 監視中の要素を管理する Map
- `statusList`: ステータスパネルの DOM 要素

#### 主要メソッド

##### `initializeObserver()`
Intersection Observer を初期化します。

```typescript
const options: IntersectionObserverInit = {
  root: null,           // ビューポートをルートとして使用
  rootMargin: '0px',    // マージンなし
  threshold: [0, 0.25, 0.5, 0.75, 1.0] // 複数の閾値
};
```

##### `handleIntersection(entries: IntersectionObserverEntry[])`
要素の交差状態が変化した際のコールバック関数です。
- 要素の表示状態を更新
- ステータスパネルを更新
- 特別な効果（アニメーション、画像読み込み）を処理

##### `updateElementVisibility(element, isVisible, ratio)`
要素の視覚的状態を更新します。
- `visible` クラスの追加/削除
- 交差比率に基づく透明度の調整

##### `handleSpecialEffects(element, entry)`
特別な効果を処理します。
- フェードインアニメーション
- 遅延画像読み込み
- カスタムイベントの発火

##### パブリックメソッド
- `addElement(element, id?)`: 新しい要素を監視に追加
- `removeElement(elementId)`: 要素を監視から削除
- `getVisibleElements()`: 現在表示中の要素を取得
- `disconnect()`: 全ての監視を停止
- `logStatus()`: デバッグ用ステータスログ出力

## 使用方法

- ページをスクロールして要素の変化を確認
- `Ctrl+L` でコンソールにステータスログを出力
- 開発者ツールでイベントやログを確認

## TypeScript の特徴

### 型定義
```typescript
interface ObservedElement {
  element: Element;
  id: string;
  isVisible: boolean;
}
```

### 厳密な型チェック
- `strict: true` による厳密な型チェック
- `noImplicitAny` でimplicit any を禁止
- `strictNullChecks` でnull/undefined チェック

### イベントハンドリング
```typescript
// カスタムイベントの型安全な処理
document.addEventListener('elementVisible', (event: Event) => {
  const customEvent = event as CustomEvent;
  console.log('Custom event fired:', customEvent.detail);
});
```

## カスタマイズ

### 新しい要素を追加
```html
<section class="observed-section" data-id="custom-section">
  <h2>カスタムセクション</h2>
  <p>この要素も監視対象になります</p>
</section>
```

### 監視オプションの変更
`initializeObserver()` メソッド内の `options` を変更：
```typescript
const options: IntersectionObserverInit = {
  root: null,
  rootMargin: '100px',  // 100px のマージンを追加
  threshold: 0.5        // 50% 表示時にトリガー
};
```

### カスタムエフェクトの追加
`handleSpecialEffects()` メソッドに新しい処理を追加できます。

## よく使われるトリガーパターン

### 1. 境界検知トリガー（threshold: 0）
要素が1pxでも画面に入った瞬間に発火。最も一般的なパターン。
- 画像の遅延読み込み開始
- アニメーションの準備
- コンテンツのプリロード

### 2. 半分表示トリガー（threshold: 0.5）
要素の50%以上が表示されたときに発火。視認性を確保したい場合。
- 動画の自動再生開始
- 重要な通知の表示
- アナリティクスイベントの送信

### 3. 完全表示トリガー（threshold: 1.0）
要素が完全に表示されたときに発火。確実に見えている状態を保証。
- 広告のインプレッションカウント
- フォームへの自動フォーカス
- 読了判定

### 4. 先読みトリガー（rootMargin使用）
要素が画面に入る前に発火。パフォーマンス最適化に有効。
```javascript
{ rootMargin: '100px' } // 100px手前で検知
{ rootMargin: '50%' }   // 画面の50%分手前で検知
```
- 次のセクションの画像プリロード
- APIからのデータ先読み
- 大きなコンポーネントの事前準備

### 5. 段階的トリガー（複数threshold）
スクロール量に応じて段階的に処理を実行。
```javascript
{ threshold: [0, 0.25, 0.5, 0.75, 1.0] }
```
- パララックス効果
- プログレスバーの更新
- 段階的なアニメーション

### 6. 離脱検知トリガー
要素が画面から出たときに発火（!entry.isIntersecting）。
- 動画の一時停止
- アニメーションの停止
- リソースの解放

## デバッグ

### コンソールログ
- 要素の交差状態の詳細情報
- 初期化メッセージ
- カスタムイベントの発火ログ

### キーボードショートカット
- `Ctrl+L`: 現在の監視状況をコンソールに出力
