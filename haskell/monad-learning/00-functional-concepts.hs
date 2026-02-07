{-
  00-functional-concepts.hs - 関数型プログラミングの考え方

  モナドを理解する前に、関数型プログラミングの核心的な概念を押さえましょう。
-}

-- ============================================================
-- 1. 純粋性（Purity）
-- ============================================================

{-
【純粋関数とは】
- 同じ入力に対して常に同じ出力を返す
- 外部状態を読まない、変更しない（副作用がない）

【純粋関数の例】
-}

-- 純粋: 入力だけで結果が決まる
add :: Int -> Int -> Int
add x y = x + y

-- 純粋: 入力が同じなら結果も同じ
double :: Int -> Int
double x = x * 2

-- 純粋: リストの長さ（リストの内容だけで決まる）
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

{-
【非純粋の例（他言語）】

JavaScript:
  let counter = 0;
  function increment() {
    counter++;  // 外部状態を変更 → 非純粋
    return counter;
  }

Python:
  import random
  def roll():
    return random.randint(1, 6)  # 外部状態に依存 → 非純粋

【なぜ純粋性が重要か】
1. テストが容易（モック不要）
2. 並列化が安全（共有状態がない）
3. 推論が容易（関数の振る舞いが入力だけで決まる）
4. キャッシュ（メモ化）が安全
-}

-- ============================================================
-- 2. 不変性（Immutability）
-- ============================================================

{-
【不変性とは】
- データは一度作成されると変更されない
- 「更新」は新しいデータの作成を意味する

【TypeScript/JavaScriptとの比較】

TypeScript (ミュータブル):
  const arr = [1, 2, 3];
  arr.push(4);  // arrを直接変更
  console.log(arr);  // [1, 2, 3, 4]

Haskell (イミュータブル):
  let arr = [1, 2, 3]
  let newArr = arr ++ [4]  -- 新しいリストを作成
  -- arr は変わらず [1, 2, 3]
  -- newArr は [1, 2, 3, 4]
-}

-- リストへの「追加」は新しいリストを作る
appendExample :: [Int]
appendExample =
  let original = [1, 2, 3]
      modified = original ++ [4]
  in modified  -- [1, 2, 3, 4]
  -- original はまだ [1, 2, 3]

-- レコードの「更新」も新しいレコードを作る
data Person = Person { name :: String, age :: Int }
  deriving (Show)

updateAge :: Person -> Int -> Person
updateAge person newAge = person { age = newAge }
-- 元のpersonは変わらない、新しいPersonが作られる

{-
【なぜ不変性が重要か】
1. 予期しない変更がない（バグの温床を減らす）
2. 並行処理が安全（ロック不要）
3. タイムトラベルデバッグが可能（過去の状態を保持）
4. 共有が安全（コピー不要）
-}

-- ============================================================
-- 3. 参照透過性（Referential Transparency）
-- ============================================================

{-
【参照透過性とは】
- 式をその評価結果で置き換えてもプログラムの意味が変わらない

例:
  let x = 2 + 3
  let y = x * x
  -- x を 5 に置き換えても同じ:
  -- let y = 5 * 5

【参照透過でない例（他言語）】

JavaScript:
  let count = 0;
  function getCount() {
    count++;
    return count;
  }
  let a = getCount();  // 1
  let b = getCount();  // 2
  // getCount()を1に置き換えると意味が変わる
-}

-- 参照透過な関数
square :: Int -> Int
square x = x * x

-- これは安全に置き換えできる
example :: Int
example =
  let a = square 5
      b = square 5
  in a + b  -- square 5 + square 5 と書いても同じ

{-
【なぜ参照透過性が重要か】
1. 等式推論ができる（コードを代数のように扱える）
2. コンパイラの最適化が効く
3. テストが容易（関数単体で検証可能）
4. 理解が容易（コードの一部だけ読めばわかる）
-}

-- ============================================================
-- 4. 高階関数（Higher-Order Functions）
-- ============================================================

{-
【高階関数とは】
- 関数を引数として受け取る、または関数を返す関数

これは関数型プログラミングの核心的な特徴
-}

-- 関数を引数として受け取る
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

twiceExample :: Int
twiceExample = applyTwice (*2) 5  -- (5 * 2) * 2 = 20

-- 関数を返す（カリー化）
addN :: Int -> (Int -> Int)
addN n = \x -> x + n

add5 :: Int -> Int
add5 = addN 5

addNExample :: Int
addNExample = add5 10  -- 15

-- 標準的な高階関数
mapExample :: [Int]
mapExample = map (*2) [1, 2, 3]  -- [2, 4, 6]

filterExample :: [Int]
filterExample = filter even [1, 2, 3, 4, 5]  -- [2, 4]

foldExample :: Int
foldExample = foldl (+) 0 [1, 2, 3, 4, 5]  -- 15（合計）

{-
【TypeScriptとの比較】

TypeScript:
  const applyTwice = <A>(f: (a: A) => A, x: A): A => f(f(x));
  const double = (x: number) => x * 2;
  applyTwice(double, 5);  // 20

Haskell:
  applyTwice (*2) 5  -- 20

共通点: 関数を値として扱える
違い: Haskellはカリー化がデフォルト、部分適用が自然
-}

-- ============================================================
-- 5. 型システムによる安全性
-- ============================================================

{-
【型で不正な状態を表現できなくする】

例: 「空でないリスト」を型で表現
-}

-- 空でないリスト型
data NonEmpty a = NonEmpty a [a]
  deriving (Show)

-- 安全なhead（必ず要素がある）
safeHead :: NonEmpty a -> a
safeHead (NonEmpty x _) = x

-- 空リストからNonEmptyは作れない（型エラー）
nonEmptyExample :: NonEmpty Int
nonEmptyExample = NonEmpty 1 [2, 3]

headResult :: Int
headResult = safeHead nonEmptyExample  -- 必ず成功、1

{-
【Maybe/Eitherによるエラー処理】

TypeScriptの場合:
  function find(arr: number[], pred: (n: number) => boolean): number | undefined {
    for (const n of arr) {
      if (pred(n)) return n;
    }
    return undefined;  // 見つからない場合
  }
  // 使う側で undefined チェックを忘れがち

Haskellの場合:
  find :: (a -> Bool) -> [a] -> Maybe a
  -- Maybe を処理しないとコンパイルエラー
  -- undefinedチェック忘れは型エラーとして検出
-}

-- ============================================================
-- 6. 関数合成（Function Composition）
-- ============================================================

{-
【関数合成とは】
小さな関数を組み合わせて大きな関数を作る

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
-}

-- 個別の関数
increment :: Int -> Int
increment x = x + 1

triple :: Int -> Int
triple x = x * 3

-- 関数合成
incrementThenTriple :: Int -> Int
incrementThenTriple = triple . increment
-- (x + 1) * 3

compositionExample :: Int
compositionExample = incrementThenTriple 5  -- (5 + 1) * 3 = 18

-- パイプライン的に読む（$ 演算子）
pipelineExample :: Int
pipelineExample = triple $ increment $ 5  -- 右から左に適用

-- &演算子で左から右に（Data.Functionから）
-- import Data.Function ((&))
-- 5 & increment & triple  -- 左から右

{-
【関数合成の利点】
1. ポイントフリースタイル（引数を明示しない）
2. 処理の流れが明確
3. 再利用性が高い
-}

-- ============================================================
-- 7. 遅延評価（Lazy Evaluation）
-- ============================================================

{-
【遅延評価とは】
- 値は実際に必要になるまで計算されない
- Haskellのデフォルト（他の多くの言語は正格評価）

【利点】
1. 無限データ構造を扱える
2. 不要な計算を避けられる
3. 制御構造を関数として定義できる
-}

-- 無限リスト（遅延評価のおかげで定義可能）
naturals :: [Int]
naturals = [0..]  -- 0, 1, 2, 3, ... 無限に続く

-- 最初の10個だけ取る（必要な分だけ計算）
firstTen :: [Int]
firstTen = take 10 naturals  -- [0,1,2,3,4,5,6,7,8,9]

-- フィボナッチ数列（無限）
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

first10Fibs :: [Integer]
first10Fibs = take 10 fibs  -- [0,1,1,2,3,5,8,13,21,34]

-- 不要な計算を避ける
lazyExample :: Int
lazyExample = head [1, error "never evaluated"]
-- 1（errorは評価されない）

{-
【注意点】
遅延評価にはメモリリークの危険もある
（計算が溜まって一気に評価されるとスペースリーク）
-}

-- ============================================================
-- 8. 代数的データ型（Algebraic Data Types）
-- ============================================================

{-
【ADTとは】
- 直積型（Product Types）: AND の関係
- 直和型（Sum Types）: OR の関係

これにより、ドメインを正確にモデル化できる
-}

-- 直積型: Person は名前 AND 年齢
data PersonADT = PersonADT String Int
  deriving (Show)

-- 直和型: Shape は Circle OR Rectangle
data Shape
  = Circle Double         -- 半径
  | Rectangle Double Double  -- 幅と高さ
  deriving (Show)

-- パターンマッチで安全に処理
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- 全ケースを処理しないと警告
-- コンパイラが網羅性をチェック

{-
【TypeScriptとの比較】

TypeScript (Tagged Union):
  type Shape =
    | { kind: 'circle'; radius: number }
    | { kind: 'rectangle'; width: number; height: number };

  function area(shape: Shape): number {
    switch (shape.kind) {
      case 'circle': return Math.PI * shape.radius ** 2;
      case 'rectangle': return shape.width * shape.height;
    }
  }

Haskellの方がより簡潔で、網羅性チェックがデフォルト
-}

-- ============================================================
-- まとめ: なぜモナドが必要か
-- ============================================================

{-
これまでの概念をすべて守りながら、副作用のある計算を扱いたい:

1. 純粋性 → 副作用を「値」として表現（IO a は「計算の記述」）
2. 不変性 → 状態変更を「新しい状態を返す関数」で表現
3. 型安全性 → Maybe/Either で失敗を型で表現
4. 関数合成 → モナドで副作用のある関数も合成可能に

モナドは:
- 「文脈を持つ計算」を抽象化
- 計算の連鎖を可能に（>>=）
- 純粋性を保ちながら「副作用的な」プログラミング

次のファイル (01-functor.hs) から、具体的にこれらの概念が
型クラスとしてどう実現されているか見ていきましょう。
-}

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Functional Programming Concepts ==="

  putStrLn "\n--- Purity ---"
  print $ add 3 5
  print $ double 7

  putStrLn "\n--- Immutability ---"
  let alice = Person "Alice" 30
  let olderAlice = updateAge alice 31
  print alice       -- 元のまま
  print olderAlice  -- 新しいPerson

  putStrLn "\n--- Higher-Order Functions ---"
  print $ applyTwice (*2) 5
  print $ map (*2) [1, 2, 3]
  print $ filter even [1..10]

  putStrLn "\n--- Function Composition ---"
  print $ incrementThenTriple 5

  putStrLn "\n--- Lazy Evaluation ---"
  print $ take 10 naturals
  print $ take 10 fibs

  putStrLn "\n--- Algebraic Data Types ---"
  print $ area (Circle 5)
  print $ area (Rectangle 4 6)
