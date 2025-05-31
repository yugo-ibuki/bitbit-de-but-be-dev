import * as E from 'fp-ts/Either';
import { pipe } from 'fp-ts/function';

// ブランド型: プリミティブ型に意味を持たせる
// 例: string型の値でも、CustomerIdとProductIdは混同できない
export type Brand<K, T> = K & { __brand: T };

// ドメイン固有の識別子型
// これらは単なるstringではなく、それぞれ異なる意味を持つ
export type CustomerId = Brand<string, 'CustomerId'>;
export type ProductId = Brand<string, 'ProductId'>;
export type OrderId = Brand<string, 'OrderId'>;
export type Email = Brand<string, 'Email'>;
export type PositiveNumber = Brand<number, 'PositiveNumber'>;

// スマートコンストラクタ: バリデーション付きの値生成関数
// Either型を返すことで、成功(Right)または失敗(Left)を表現
export const CustomerId = {
  of: (value: string): E.Either<string, CustomerId> =>
    value.length > 0
      ? E.right(value as CustomerId)
      : E.left('CustomerId cannot be empty'),
};

export const ProductId = {
  of: (value: string): E.Either<string, ProductId> =>
    value.length > 0
      ? E.right(value as ProductId)
      : E.left('ProductId cannot be empty'),
};

export const OrderId = {
  of: (value: string): E.Either<string, OrderId> =>
    value.length > 0
      ? E.right(value as OrderId)
      : E.left('OrderId cannot be empty'),
};

// メールアドレスのバリデーション
// 正規表現でフォーマットをチェックし、不正な値は作れないようにする
export const Email = {
  of: (value: string): E.Either<string, Email> => {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(value)
      ? E.right(value as Email)
      : E.left('Invalid email format');
  },
};

// 正の数のみを許可する型
// ビジネスルール：価格や数量は必ず正の数
export const PositiveNumber = {
  of: (value: number): E.Either<string, PositiveNumber> =>
    value > 0
      ? E.right(value as PositiveNumber)
      : E.left('Number must be positive'),
};

// 金額を表す値オブジェクト
// 金額と通貨を一体として扱うことで、通貨の混在を防ぐ
export type Money = {
  readonly amount: PositiveNumber;
  readonly currency: Currency;
};

// サポートする通貨の型
// Union型で限定することで、不正な通貨コードを防ぐ
export type Currency = 'USD' | 'EUR' | 'JPY';

// Moneyのスマートコンストラクタ
// 金額のバリデーションを含む
export const Money = {
  of: (amount: number, currency: Currency): E.Either<string, Money> =>
    pipe(
      PositiveNumber.of(amount),
      E.map((amount) => ({ amount, currency }))
    ),
};

// 商品エンティティ
// readonlyで不変性を保証
export type Product = {
  readonly id: ProductId;
  readonly name: string;
  readonly price: Money;
};

// 注文明細行
// 商品と数量の組み合わせ
export type OrderLine = {
  readonly product: Product;
  readonly quantity: PositiveNumber;
};

// 注文ステータス（代数的データ型）
// タグ付きユニオンで状態遷移を型安全に表現
// 各状態が持つデータも型で定義
export type OrderStatus = 
  | { readonly tag: 'Pending' }                                    // 保留中
  | { readonly tag: 'Confirmed' }                                  // 確認済み
  | { readonly tag: 'Shipped'; readonly trackingNumber: string }   // 発送済み（追跡番号付き）
  | { readonly tag: 'Delivered'; readonly deliveredAt: Date }      // 配達完了（配達日時付き）
  | { readonly tag: 'Cancelled'; readonly reason: string };        // キャンセル（理由付き）

// 注文エンティティ
// ドメインの中心となる集約ルート
export type Order = {
  readonly id: OrderId;
  readonly customerId: CustomerId;
  readonly lines: ReadonlyArray<OrderLine>;  // 不変の配列
  readonly status: OrderStatus;
  readonly createdAt: Date;
};

// 顧客エンティティ
export type Customer = {
  readonly id: CustomerId;
  readonly email: Email;
  readonly name: string;
};