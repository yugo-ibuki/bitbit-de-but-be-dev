import * as E from 'fp-ts/Either';
import * as O from 'fp-ts/Option';
import { pipe } from 'fp-ts/function';
import {
  Order,
  OrderLine,
  OrderStatus,
  Money,
  PositiveNumber,
  Product,
  CustomerId,
  OrderId,
  Currency,
} from './types';

// 注文明細行の合計金額を計算
// 純粋関数：同じ入力に対して常に同じ出力を返す
export const calculateLineTotal = (line: OrderLine): Money => ({
  amount: (line.product.price.amount * line.quantity) as PositiveNumber,
  currency: line.product.price.currency,
});

// 注文全体の合計金額を計算
// ビジネスルール：
// 1. 注文には最低1つの明細行が必要
// 2. すべての商品は同じ通貨でなければならない
export const calculateOrderTotal = (
  lines: ReadonlyArray<OrderLine>
): E.Either<string, Money> => {
  // 空の注文は許可しない
  if (lines.length === 0) {
    return E.left('Order must have at least one line');
  }

  // 通貨の一貫性をチェック
  const firstCurrency = lines[0].product.price.currency;
  const allSameCurrency = lines.every(
    (line) => line.product.price.currency === firstCurrency
  );

  if (!allSameCurrency) {
    return E.left('All products must have the same currency');
  }

  // 合計金額を計算
  const total = lines.reduce(
    (sum, line) => sum + calculateLineTotal(line).amount,
    0
  );

  return Money.of(total, firstCurrency);
};

// 注文に商品を追加
// ビジネスルール：保留中の注文にのみ商品を追加できる
// 同じ商品が既にある場合は数量を増やす
export const addProductToOrder = (
  order: Order,
  product: Product,
  quantity: number
): E.Either<string, Order> => {
  // 状態チェック：保留中のみ変更可能
  if (order.status.tag !== 'Pending') {
    return E.left('Can only add products to pending orders');
  }

  return pipe(
    // 数量のバリデーション
    PositiveNumber.of(quantity),
    E.map((qty) => {
      // 既存の商品を探す
      const existingLineIndex = order.lines.findIndex(
        (line) => line.product.id === product.id
      );

      if (existingLineIndex >= 0) {
        // 既存商品の数量を更新（イミュータブルに）
        const updatedLines = [...order.lines];
        updatedLines[existingLineIndex] = {
          ...updatedLines[existingLineIndex],
          quantity: (updatedLines[existingLineIndex].quantity + qty) as PositiveNumber,
        };
        return { ...order, lines: updatedLines };
      } else {
        // 新しい商品を追加
        const newLine: OrderLine = { product, quantity: qty };
        return { ...order, lines: [...order.lines, newLine] };
      }
    })
  );
};

export const removeProductFromOrder = (
  order: Order,
  productId: Product['id']
): E.Either<string, Order> => {
  if (order.status.tag !== 'Pending') {
    return E.left('Can only remove products from pending orders');
  }

  const filteredLines = order.lines.filter(
    (line) => line.product.id !== productId
  );

  if (filteredLines.length === 0) {
    return E.left('Order must have at least one line');
  }

  return E.right({ ...order, lines: filteredLines });
};

// 注文を確認する（状態遷移: Pending → Confirmed）
// ビジネスルール：
// 1. 保留中の注文のみ確認可能
// 2. 空の注文は確認できない
export const confirmOrder = (order: Order): E.Either<string, Order> => {
  if (order.status.tag !== 'Pending') {
    return E.left('Only pending orders can be confirmed');
  }

  if (order.lines.length === 0) {
    return E.left('Cannot confirm empty order');
  }

  const confirmedStatus: OrderStatus = { tag: 'Confirmed' };
  return E.right({ ...order, status: confirmedStatus });
};

// 注文を発送する（状態遷移: Confirmed → Shipped）
// 追跡番号が必須
export const shipOrder = (
  order: Order,
  trackingNumber: string
): E.Either<string, Order> => {
  if (order.status.tag !== 'Confirmed') {
    return E.left('Only confirmed orders can be shipped');
  }

  if (trackingNumber.length === 0) {
    return E.left('Tracking number is required');
  }

  const shippedStatus: OrderStatus = { tag: 'Shipped', trackingNumber };
  return E.right({ ...order, status: shippedStatus });
};

// 注文を配達完了にする（状態遷移: Shipped → Delivered）
// 配達日時を自動的に記録
export const deliverOrder = (order: Order): E.Either<string, Order> => {
  if (order.status.tag !== 'Shipped') {
    return E.left('Only shipped orders can be delivered');
  }

  const deliveredStatus: OrderStatus = {
    tag: 'Delivered',
    deliveredAt: new Date(),
  };
  return E.right({ ...order, status: deliveredStatus });
};

// 注文をキャンセルする
// ビジネスルール：
// 1. 配達済みの注文はキャンセル不可
// 2. 既にキャンセル済みの注文は再度キャンセル不可
// 3. キャンセル理由が必須
export const cancelOrder = (
  order: Order,
  reason: string
): E.Either<string, Order> => {
  if (order.status.tag === 'Delivered') {
    return E.left('Cannot cancel delivered orders');
  }

  if (order.status.tag === 'Cancelled') {
    return E.left('Order is already cancelled');
  }

  if (reason.length === 0) {
    return E.left('Cancellation reason is required');
  }

  const cancelledStatus: OrderStatus = { tag: 'Cancelled', reason };
  return E.right({ ...order, status: cancelledStatus });
};

export const createEmptyOrder = (
  customerId: CustomerId,
  orderId: OrderId
): Order => ({
  id: orderId,
  customerId,
  lines: [],
  status: { tag: 'Pending' },
  createdAt: new Date(),
});

export const applyDiscount = (
  money: Money,
  discountPercentage: number
): E.Either<string, Money> => {
  if (discountPercentage < 0 || discountPercentage > 100) {
    return E.left('Discount percentage must be between 0 and 100');
  }

  const discountedAmount = money.amount * (1 - discountPercentage / 100);
  return Money.of(discountedAmount, money.currency);
};

export const findMostExpensiveProduct = (
  products: ReadonlyArray<Product>
): O.Option<Product> => {
  if (products.length === 0) {
    return O.none;
  }
  
  const sorted = [...products].sort((a, b) => b.price.amount - a.price.amount);
  return O.some(sorted[0]);
};

export const filterProductsByPriceRange = (
  products: ReadonlyArray<Product>,
  minPrice: number,
  maxPrice: number,
  currency: Currency
): ReadonlyArray<Product> =>
  products.filter(
    (product) =>
      product.price.currency === currency &&
      product.price.amount >= minPrice &&
      product.price.amount <= maxPrice
  );