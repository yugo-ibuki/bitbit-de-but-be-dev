# 関数型プログラミングの主要概念

## モナド (Monad)

モナドは、値をコンテキスト（文脈）でラップし、そのコンテキストを保ちながら値を変換するための仕組みです。

### 特徴
- **flatMap/chain**: ネストした構造をフラットにしながら値を変換
- **of/return**: 値をモナドでラップ
- **結合則と単位元の法則**を満たす

### 例
```typescript
// fp-tsのEitherモナド
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'

const divide = (a: number, b: number): E.Either<string, number> =>
  b === 0 ? E.left('ゼロ除算エラー') : E.right(a / b)

const result = pipe(
  E.right(10),
  E.chain(x => divide(x, 2)),  // flatMap
  E.chain(x => divide(x, 2.5))
)
// Right(2)
```

## ファンクター (Functor)

ファンクターは、コンテキストの中の値に関数を適用できる構造です。

### 特徴
- **map**: コンテキストを保ちながら中の値を変換
- **恒等関数の法則**と**合成の法則**を満たす

### 例
```typescript
// 配列はファンクター
const numbers = [1, 2, 3]
const doubled = numbers.map(x => x * 2)  // [2, 4, 6]

// fp-tsのOption
import * as O from 'fp-ts/Option'
const maybeValue = O.some(5)
const result = pipe(
  maybeValue,
  O.map(x => x * 2)  // Some(10)
)
```

## Either型

Either型は、成功（Right）または失敗（Left）のどちらかを表す型です。

### 特徴
- **エラーハンドリング**に使用
- **Left**: エラーや失敗を表す
- **Right**: 成功した値を表す

### 例
```typescript
import * as E from 'fp-ts/Either'

type ValidationError = string

const validateAge = (age: number): E.Either<ValidationError, number> => {
  if (age < 0) return E.left('年齢は0以上である必要があります')
  if (age > 150) return E.left('年齢が大きすぎます')
  return E.right(age)
}

const result = validateAge(25)  // Right(25)
const error = validateAge(-5)   // Left('年齢は0以上である必要があります')
```

## Option/Maybe型

Option（Maybe）型は、値が存在するか存在しないかを表す型です。

### 特徴
- **null/undefinedの安全な扱い**
- **Some/Just**: 値が存在する
- **None/Nothing**: 値が存在しない

### 例
```typescript
import * as O from 'fp-ts/Option'

const findUser = (id: string): O.Option<User> => {
  const user = database.find(u => u.id === id)
  return user ? O.some(user) : O.none
}

const result = pipe(
  findUser('123'),
  O.map(user => user.name),
  O.getOrElse(() => 'ユーザーが見つかりません')
)
```

## その他の重要な概念

### アプリカティブファンクター (Applicative Functor)
複数の値をコンテキスト内で組み合わせる
```typescript
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

const add = (a: number) => (b: number) => a + b
const result = pipe(
  O.some(add),
  O.ap(O.some(2)),
  O.ap(O.some(3))
)  // Some(5)
```

### セミグループとモノイド (Semigroup & Monoid)
- **セミグループ**: 結合的な二項演算を持つ
- **モノイド**: セミグループ + 単位元

```typescript
import * as S from 'fp-ts/string'
import * as M from 'fp-ts/Monoid'

const stringMonoid = S.Monoid
const result = M.concatAll(stringMonoid)(['Hello', ' ', 'World'])
// 'Hello World'
```

### パイプとコンポジション
関数を組み合わせて新しい関数を作る
```typescript
import { pipe, flow } from 'fp-ts/function'

// pipe: 左から右へ
const result1 = pipe(
  5,
  x => x * 2,
  x => x + 1
)  // 11

// flow: 関数の合成
const calculate = flow(
  (x: number) => x * 2,
  x => x + 1
)
const result2 = calculate(5)  // 11
```

## まとめ

これらの概念により、関数型プログラミングでは：
- **副作用の分離**: 純粋関数とエフェクトの明確な分離
- **型安全性**: コンパイル時にエラーを検出
- **合成可能性**: 小さな関数を組み合わせて複雑な処理を構築
- **宣言的なコード**: 「何をするか」を記述（「どうやってするか」ではなく）

が実現されます。