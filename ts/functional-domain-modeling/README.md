# 関数型ドメインモデリング入門

このプロジェクトは、TypeScriptとfp-tsを使用した関数型ドメインモデリングの実践例です。ECサイトの注文システムを題材に、関数型プログラミングの原則を適用したドメインモデリングを学べます。

## 🎯 プロジェクトの目的

関数型プログラミングの手法を使って、以下を実現します：
- **型安全性**: コンパイル時にエラーを検出
- **ビジネスルールの表現**: 不正な状態を作れないようにする
- **保守性**: 純粋関数による予測可能なコード
- **エラーハンドリング**: 例外ではなく型でエラーを表現

## 📚 主要な概念

### 1. ブランド型（Branded Types）
プリミティブ型に意味を持たせ、型の混同を防ぎます：

```typescript
// ❌ 悪い例：IDが混同される可能性
function processOrder(customerId: string, orderId: string) { }

// ✅ 良い例：型レベルで区別
type CustomerId = Brand<string, 'CustomerId'>;
type OrderId = Brand<string, 'OrderId'>;
function processOrder(customerId: CustomerId, orderId: OrderId) { }
```

### 2. スマートコンストラクタ
バリデーション付きの値生成関数で、不正な値の作成を防ぎます：

```typescript
// メールアドレスの生成（バリデーション付き）
const emailResult = Email.of('user@example.com');
// Either<string, Email> 型が返される

if (E.isRight(emailResult)) {
  // 正しいメールアドレス
  const email = emailResult.right;
} else {
  // エラーメッセージ
  console.error(emailResult.left); // "Invalid email format"
}
```

### 3. Either型によるエラーハンドリング
例外を投げる代わりに、エラーを型で表現します：

```typescript
// Either<エラーの型, 成功値の型>
type Result = Either<string, Order>;

// Left = エラー、Right = 成功
const result = addProductToOrder(order, product, -1);
// Left("Number must be positive")
```

### 4. 代数的データ型（ADT）
注文の状態を型安全に表現：

```typescript
type OrderStatus = 
  | { tag: 'Pending' }                                    // 保留中
  | { tag: 'Confirmed' }                                  // 確認済み
  | { tag: 'Shipped'; trackingNumber: string }           // 発送済み
  | { tag: 'Delivered'; deliveredAt: Date }              // 配達完了

// タグでパターンマッチング
switch (order.status.tag) {
  case 'Pending':
    // 商品の追加が可能
    break;
  case 'Shipped':
    // 追跡番号を表示: order.status.trackingNumber
    break;
}
```

### 5. 純粋関数とイミュータビリティ
すべての関数は副作用なし、新しいオブジェクトを返します：

```typescript
// ❌ 悪い例：元のオブジェクトを変更
function addProduct(order: Order, product: Product) {
  order.lines.push({ product, quantity: 1 }); // 破壊的変更
}

// ✅ 良い例：新しいオブジェクトを返す
function addProduct(order: Order, product: Product): Either<string, Order> {
  return E.right({
    ...order,
    lines: [...order.lines, { product, quantity: 1 }]
  });
}
```

### 6. 関数合成（Function Composition）
`pipe`を使った処理の組み立て：

```typescript
const result = pipe(
  emptyOrder,
  (order) => addProductToOrder(order, laptop, 1),
  E.chain((order) => addProductToOrder(order, mouse, 2)),
  E.chain((order) => confirmOrder(order)),
  E.chain((order) => shipOrder(order, 'TRACK-123'))
);
```

## 🏗️ ドメインモデルの構造

### エンティティと値オブジェクト
```
Order (注文)
├── id: OrderId
├── customerId: CustomerId
├── lines: OrderLine[] (注文明細)
│   ├── product: Product
│   └── quantity: PositiveNumber
├── status: OrderStatus (状態)
└── createdAt: Date

Product (商品)
├── id: ProductId
├── name: string
└── price: Money
    ├── amount: PositiveNumber
    └── currency: Currency
```

### ビジネスルール
1. **注文の状態遷移**
   - Pending → Confirmed → Shipped → Delivered
   - いつでもCancelledに遷移可能（Delivered以外）

2. **制約**
   - 商品の追加・削除はPending状態のみ
   - 空の注文は確認できない
   - すべての商品は同じ通貨である必要がある
   - 価格と数量は正の数のみ

## 🚀 使い方

```bash
# 依存関係のインストール
npm install

# 実行例を見る
npm run dev

# 型チェック
npm run typecheck

# ビルド
npm run build
```

## 📁 ファイル構成

- `src/domain/types.ts` - ドメイン型と値オブジェクトの定義
- `src/domain/operations.ts` - ビジネスロジック（純粋関数）
- `src/index.ts` - 実行例とエラーハンドリングのデモ

## 💡 学習のポイント

1. **型を活用する**: 文字列や数値ではなく、意味のある型を使う
2. **バリデーション**: 不正な値を作れないようにする
3. **エラーを値として扱う**: try-catchではなくEither型を使う
4. **状態を明示的に**: 代数的データ型で状態を表現
5. **副作用を避ける**: 純粋関数でビジネスロジックを実装

## 📖 参考資料

- [fp-ts ドキュメント](https://gcanti.github.io/fp-ts/)
- [Domain Modeling Made Functional (書籍)](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/)
- [関数型プログラミングの基礎](https://github.com/gcanti/fp-ts/blob/master/docs/learning-resources.md)