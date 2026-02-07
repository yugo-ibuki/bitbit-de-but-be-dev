{-
  02-applicative.hs - Applicative Functor の理解

  Applicativeは「文脈の中にある関数」を「文脈の中にある値」に適用できる。
  Functorの強化版で、複数の引数を持つ関数をコンテナ内で使える。
-}

-- ============================================================
-- Applicative 型クラスの定義（概念理解用）
-- ============================================================

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  読み方:
  - pure: 値を文脈（コンテナ）に入れる
  - <*>: 文脈内の関数を文脈内の値に適用する

  重要: Applicative は Functor を継承している
        すべての Applicative は Functor でもある
-}

-- ============================================================
-- Functorの限界とApplicativeの必要性
-- ============================================================

{-
【問題】2引数の関数をMaybeの中の値に適用したい

例: add x y = x + y を Just 3 と Just 5 に適用したい

Functorだけでは:
  fmap add (Just 3)  -- 結果: Just (add 3) = Just (\y -> 3 + y)
                     -- 型: Maybe (Int -> Int)
  -- ここで詰まる！関数がMaybeの中にある

Applicativeがあれば:
  pure add <*> Just 3 <*> Just 5  -- 結果: Just 8
  -- または
  add <$> Just 3 <*> Just 5      -- 結果: Just 8
-}

-- ============================================================
-- 他言語との比較
-- ============================================================

{-
【TypeScript での近似】

TypeScriptには直接のApplicativeはないが、概念的には:

// Haskell: add <$> Just 3 <*> Just 5
// TypeScript (概念的な等価物):
const maybeAdd = (a: number | null, b: number | null): number | null => {
  if (a === null || b === null) return null;
  return a + b;
};

違い:
- TypeScriptでは各組み合わせを手動でハンドリング
- Haskellでは <*> が自動的に処理

【Rust での近似】

Rustでは Option の and_then や zip を使う:
  Some(3).zip(Some(5)).map(|(a, b)| a + b)

Haskellはより簡潔:
  (+) <$> Just 3 <*> Just 5
-}

-- ============================================================
-- pure の理解
-- ============================================================

-- pure は値を「最小の文脈」に入れる
pureList :: [Int]
pureList = pure 5
-- 結果: [5]（単一要素のリスト）

pureMaybe :: Maybe Int
pureMaybe = pure 5
-- 結果: Just 5

pureEither :: Either String Int
pureEither = pure 5
-- 結果: Right 5

-- ============================================================
-- <*> (アプライ演算子) の理解
-- ============================================================

-- Maybe での <*>
applyExample1 :: Maybe Int
applyExample1 = Just (+3) <*> Just 5
-- 結果: Just 8

applyExample2 :: Maybe Int
applyExample2 = Just (+3) <*> Nothing
-- 結果: Nothing（値がないので適用できない）

applyExample3 :: Maybe Int
applyExample3 = Nothing <*> Just 5
-- 結果: Nothing（関数がないので適用できない）

-- ============================================================
-- 複数引数の関数への適用
-- ============================================================

-- 2引数の関数
add :: Int -> Int -> Int
add x y = x + y

twoArgExample :: Maybe Int
twoArgExample = add <$> Just 3 <*> Just 5
-- 結果: Just 8
-- 分解:
--   add <$> Just 3     -- Just (add 3) = Just (\y -> 3 + y)
--   Just (\y -> ...) <*> Just 5  -- Just 8

-- 3引数の関数
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

threeArgExample :: Maybe Int
threeArgExample = addThree <$> Just 1 <*> Just 2 <*> Just 3
-- 結果: Just 6

-- どれかがNothingなら結果もNothing
failExample :: Maybe Int
failExample = addThree <$> Just 1 <*> Nothing <*> Just 3
-- 結果: Nothing

-- ============================================================
-- List の Applicative
-- ============================================================

{-
ListのApplicativeは「すべての組み合わせ」を計算する
これは非決定性計算と見なせる
-}

listApply1 :: [Int]
listApply1 = [(+1), (*2)] <*> [1, 2, 3]
-- 結果: [2,3,4,2,4,6]
-- 説明: (+1)を[1,2,3]に適用 → [2,3,4]
--       (*2)を[1,2,3]に適用 → [2,4,6]
--       結果を連結

listApply2 :: [Int]
listApply2 = (+) <$> [1, 2] <*> [10, 100]
-- 結果: [11, 101, 12, 102]
-- すべての組み合わせ: 1+10, 1+100, 2+10, 2+100

-- ============================================================
-- liftA2: 便利な関数
-- ============================================================

import Control.Applicative (liftA2)

-- liftA2 f x y = f <$> x <*> y と同じ
liftExample :: Maybe Int
liftExample = liftA2 (+) (Just 3) (Just 5)
-- 結果: Just 8

-- リストでの liftA2
liftListExample :: [Int]
liftListExample = liftA2 (*) [1, 2] [10, 100]
-- 結果: [10, 100, 20, 200]

-- ============================================================
-- 実用例: バリデーション
-- ============================================================

-- ユーザー入力をパースする（失敗するかもしれない）
parseAge :: String -> Maybe Int
parseAge s = case reads s of
  [(n, "")] | n >= 0 && n < 150 -> Just n
  _ -> Nothing

parseName :: String -> Maybe String
parseName s = if null s then Nothing else Just s

-- Person型を作る
data Person = Person String Int
  deriving (Show)

-- Applicativeでバリデーションを組み合わせる
mkPerson :: String -> String -> Maybe Person
mkPerson name age = Person <$> parseName name <*> parseAge age

-- 使用例
person1 :: Maybe Person
person1 = mkPerson "Alice" "30"
-- 結果: Just (Person "Alice" 30)

person2 :: Maybe Person
person2 = mkPerson "" "30"
-- 結果: Nothing（名前が空）

person3 :: Maybe Person
person3 = mkPerson "Bob" "invalid"
-- 結果: Nothing（年齢が不正）

-- ============================================================
-- Applicativeの法則
-- ============================================================

{-
1. Identity（恒等）
   pure id <*> v = v

2. Composition（合成）
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

3. Homomorphism（準同型）
   pure f <*> pure x = pure (f x)

4. Interchange（交換）
   u <*> pure y = pure ($ y) <*> u
-}

-- ============================================================
-- なぜApplicativeが重要か？
-- ============================================================

{-
1. 並列計算の可能性
   - Applicativeは「独立した計算」を表現
   - a <*> b において、aとbは互いに依存しない
   - 理論上、並列実行が可能

2. バリデーションの組み合わせ
   - 複数のバリデーションを組み合わせられる
   - すべての検証を実行し、すべてのエラーを集められる
   (ValidationというApplicativeを使う場合)

3. パーサーコンビネータ
   - パーサーを組み合わせて複雑なパーサーを作る
   - Applicativeスタイルは非常に読みやすい
-}

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次の結果を予想してください
q1 :: Maybe Int
q1 = (*) <$> Just 4 <*> Just 5
-- 答え: ???

-- Q2: 次の結果を予想してください（リストの全組み合わせ）
q2 :: [Int]
q2 = (+) <$> [1, 2] <*> [10, 20, 30]
-- 答え: ???

-- Q3: 次の結果を予想してください
q3 :: Maybe Int
q3 = liftA2 max (Just 10) (Just 20)
-- 答え: ???

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Applicative Examples ==="

  putStrLn "\n--- pure ---"
  print (pure 5 :: Maybe Int)
  print (pure 5 :: [Int])

  putStrLn "\n--- <*> with Maybe ---"
  print $ Just (+3) <*> Just 5
  print $ Just (+3) <*> Nothing
  print $ (Nothing :: Maybe (Int -> Int)) <*> Just 5

  putStrLn "\n--- Multiple arguments ---"
  print $ add <$> Just 3 <*> Just 5
  print $ addThree <$> Just 1 <*> Just 2 <*> Just 3

  putStrLn "\n--- List Applicative ---"
  print $ [(+1), (*2)] <*> [1, 2, 3]
  print $ (+) <$> [1, 2] <*> [10, 100]

  putStrLn "\n--- Validation Example ---"
  print $ mkPerson "Alice" "30"
  print $ mkPerson "" "30"
  print $ mkPerson "Bob" "invalid"

  putStrLn "\n--- 練習問題の答え ---"
  print q1  -- Just 20
  print q2  -- [11,21,31,12,22,32]
  print q3  -- Just 20
