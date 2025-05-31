import * as E from 'fp-ts/Either';
import * as O from 'fp-ts/Option';
import { pipe } from 'fp-ts/function';
import {
  CustomerId,
  ProductId,
  OrderId,
  Email,
  Money,
  Product,
  Customer,
} from './domain/types';
import {
  createEmptyOrder,
  addProductToOrder,
  confirmOrder,
  shipOrder,
  deliverOrder,
  calculateOrderTotal,
  applyDiscount,
  findMostExpensiveProduct,
  filterProductsByPriceRange,
} from './domain/operations';

const example = () => {
  console.log('=== Functional Domain Modeling Example ===\n');

  // Do記法を使った関数型エラーハンドリング
  // 各ステップでEither型を返し、失敗時は自動的に処理を中断
  const customerResult = pipe(
    E.Do,
    E.bind('id', () => CustomerId.of('CUST-001')),
    E.bind('email', () => Email.of('john.doe@example.com')),
    E.map(({ id, email }): Customer => ({
      id,
      email,
      name: 'John Doe',
    }))
  );

  // Eitherのパターンマッチング
  if (E.isLeft(customerResult)) {
    console.error('Failed to create customer:', customerResult.left);
    return;
  }

  const customer = customerResult.right;
  console.log('Customer created:', customer);

  const productsResult = pipe(
    E.Do,
    E.bind('laptop', () =>
      pipe(
        E.Do,
        E.bind('id', () => ProductId.of('PROD-001')),
        E.bind('price', () => Money.of(1299.99, 'USD')),
        E.map(({ id, price }): Product => ({
          id,
          name: 'MacBook Pro',
          price,
        }))
      )
    ),
    E.bind('mouse', () =>
      pipe(
        E.Do,
        E.bind('id', () => ProductId.of('PROD-002')),
        E.bind('price', () => Money.of(79.99, 'USD')),
        E.map(({ id, price }): Product => ({
          id,
          name: 'Magic Mouse',
          price,
        }))
      )
    ),
    E.bind('keyboard', () =>
      pipe(
        E.Do,
        E.bind('id', () => ProductId.of('PROD-003')),
        E.bind('price', () => Money.of(149.99, 'USD')),
        E.map(({ id, price }): Product => ({
          id,
          name: 'Magic Keyboard',
          price,
        }))
      )
    )
  );

  if (E.isLeft(productsResult)) {
    console.error('Failed to create products:', productsResult.left);
    return;
  }

  const { laptop, mouse, keyboard } = productsResult.right;
  const allProducts = [laptop, mouse, keyboard];

  console.log('\nProducts catalog:');
  allProducts.forEach((p) =>
    console.log(`- ${p.name}: $${p.price.amount} ${p.price.currency}`)
  );

  const mostExpensive = findMostExpensiveProduct(allProducts);
  pipe(
    mostExpensive,
    O.fold(
      () => console.log('\nNo products found'),
      (product) =>
        console.log(
          `\nMost expensive product: ${product.name} ($${product.price.amount})`
        )
    )
  );

  const affordableProducts = filterProductsByPriceRange(
    allProducts,
    50,
    150,
    'USD'
  );
  console.log('\nAffordable products (USD 50-150):');
  affordableProducts.forEach((p) =>
    console.log(`- ${p.name}: $${p.price.amount}`)
  );

  const orderIdResult = OrderId.of('ORD-001');
  if (E.isLeft(orderIdResult)) {
    console.error('Failed to create order ID:', orderIdResult.left);
    return;
  }

  console.log('\n=== Order Processing Workflow ===\n');

  // 空の注文を作成（初期状態はPending）
  const emptyOrder = createEmptyOrder(customer.id, orderIdResult.right);
  console.log('1. Empty order created:', {
    id: emptyOrder.id,
    status: emptyOrder.status,
  });

  // パイプラインで商品を追加
  // E.chainは前の操作が成功した場合のみ次の操作を実行
  const orderWithProducts = pipe(
    emptyOrder,
    (order) => addProductToOrder(order, laptop, 1),
    E.chain((order) => addProductToOrder(order, mouse, 2)),
    E.chain((order) => addProductToOrder(order, keyboard, 1))
  );

  if (E.isLeft(orderWithProducts)) {
    console.error('Failed to add products:', orderWithProducts.left);
    return;
  }

  console.log('\n2. Products added to order:');
  orderWithProducts.right.lines.forEach((line) =>
    console.log(
      `   - ${line.product.name} x${line.quantity} = $${
        line.product.price.amount * line.quantity
      }`
    )
  );

  const orderTotal = calculateOrderTotal(orderWithProducts.right.lines);
  pipe(
    orderTotal,
    E.fold(
      (error) => console.error('Failed to calculate total:', error),
      (total) =>
        console.log(`   Total: $${total.amount} ${total.currency}`)
    )
  );

  pipe(
    orderTotal,
    E.chain((total) => applyDiscount(total, 10)),
    E.fold(
      (error) => console.error('Failed to apply discount:', error),
      (discounted) =>
        console.log(
          `   Total with 10% discount: $${discounted.amount} ${discounted.currency}`
        )
    )
  );

  const confirmedOrder = pipe(
    orderWithProducts,
    E.chain(confirmOrder)
  );

  if (E.isLeft(confirmedOrder)) {
    console.error('Failed to confirm order:', confirmedOrder.left);
    return;
  }

  console.log('\n3. Order confirmed:', confirmedOrder.right.status);

  const shippedOrder = pipe(
    confirmedOrder,
    E.chain((order) => shipOrder(order, 'TRACK-123456'))
  );

  if (E.isLeft(shippedOrder)) {
    console.error('Failed to ship order:', shippedOrder.left);
    return;
  }

  console.log('\n4. Order shipped:', shippedOrder.right.status);

  const deliveredOrder = pipe(shippedOrder, E.chain(deliverOrder));

  if (E.isLeft(deliveredOrder)) {
    console.error('Failed to deliver order:', deliveredOrder.left);
    return;
  }

  console.log('\n5. Order delivered:', deliveredOrder.right.status);

  console.log('\n=== Demonstrating Error Handling ===\n');

  // 不正なメールアドレスのバリデーション
  const invalidEmail = Email.of('not-an-email');
  console.log(
    'Invalid email result:',
    E.isLeft(invalidEmail) ? invalidEmail.left : 'Success'
  );

  // 負の金額のバリデーション
  const negativePrice = Money.of(-100, 'USD');
  console.log(
    'Negative price result:',
    E.isLeft(negativePrice) ? negativePrice.left : 'Success'
  );

  // 不正な状態遷移の防止
  // Pending状態の注文を直接Shippedにしようとする
  const attemptToShipPendingOrder = shipOrder(emptyOrder, 'TRACK-789');
  console.log(
    'Ship pending order result:',
    E.isLeft(attemptToShipPendingOrder)
      ? attemptToShipPendingOrder.left
      : 'Success'
  );
};

example();