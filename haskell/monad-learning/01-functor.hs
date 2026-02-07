{-
  01-functor.hs - Functor（関手）の理解

  Functorは「写像可能な構造」を表す型クラスです。
  コンテナの中身に関数を適用する能力を抽象化しています。
-}

-- ============================================================
-- Functor 型クラスの定義（概念理解用）
-- ============================================================

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  読み方:
  - fは「型コンストラクタ」（Maybe, [], Either a など）
  - fmapは「(a -> b)の関数」と「f a」を取り、「f b」を返す
-}

-- ============================================================
-- 他言語との比較
-- ============================================================

{-
【TypeScript との比較】

TypeScript:
  const nums = [1, 2, 3];
  const doubled = nums.map(x => x * 2);  // [2, 4, 6]

Haskell:
  fmap (*2) [1, 2, 3]  -- [2, 4, 6]

違い:
- TypeScriptの map は Array のメソッド
- Haskellの fmap は任意のFunctorに適用可能（汎用的）

【Rust との比較】

Rust:
  let nums = vec![1, 2, 3];
  let doubled: Vec<_> = nums.iter().map(|x| x * 2).collect();

Haskell:
  fmap (*2) [1, 2, 3]

違い:
- Rustはイテレータベース、collect()で具体型に変換
- Haskellは直接Functorインスタンスに作用
-}

-- ============================================================
-- Functor の法則（Laws）
-- ============================================================

{-
Functorインスタンスは以下の法則を満たす必要がある:

1. 恒等法則（Identity Law）
   fmap id == id
   「何もしない関数をfmapしても、何も変わらない」

2. 合成法則（Composition Law）
   fmap (f . g) == fmap f . fmap g
   「合成関数のfmapは、fmapの合成と等しい」

これらの法則が保証するもの:
- 予測可能な振る舞い
- リファクタリングの安全性
- 最適化の可能性
-}

-- ============================================================
-- 実例: List Functor
-- ============================================================

-- リストに対するfmap（実質的にmap関数と同じ）
listExample :: [Int]
listExample = fmap (*2) [1, 2, 3, 4, 5]
-- 結果: [2, 4, 6, 8, 10]

-- 関数合成との組み合わせ
composedExample :: [Int]
composedExample = fmap ((*2) . (+1)) [1, 2, 3]
-- 結果: [4, 6, 8]  -- (1+1)*2, (2+1)*2, (3+1)*2

-- ============================================================
-- 実例: Maybe Functor
-- ============================================================

-- Maybe は「値があるかもしれない」を表す
maybeExample1 :: Maybe Int
maybeExample1 = fmap (*2) (Just 5)
-- 結果: Just 10

maybeExample2 :: Maybe Int
maybeExample2 = fmap (*2) Nothing
-- 結果: Nothing（安全に伝播される）

-- ============================================================
-- 実例: Either Functor
-- ============================================================

-- Either はエラーか成功かを表す
-- fmap は Right（成功）側にのみ作用する
eitherExample1 :: Either String Int
eitherExample1 = fmap (*2) (Right 5)
-- 結果: Right 10

eitherExample2 :: Either String Int
eitherExample2 = fmap (*2) (Left "error occurred")
-- 結果: Left "error occurred"（エラーはそのまま）

-- ============================================================
-- <$> 演算子（fmapの中置版）
-- ============================================================

-- fmap と <$> は同じ
-- fmap f x  ==  f <$> x

infixExample1 :: Maybe Int
infixExample1 = (*2) <$> Just 5
-- 結果: Just 10

infixExample2 :: [Int]
infixExample2 = (*2) <$> [1, 2, 3]
-- 結果: [2, 4, 6]

-- ============================================================
-- なぜFunctorが重要か？
-- ============================================================

{-
1. 抽象化の力
   - 「コンテナの中身を変換する」という概念を統一
   - Maybe, List, Either, IO... 全て同じfmapで操作

2. 安全性
   - Nothingに対するfmapは安全にNothingを返す
   - Leftに対するfmapはエラーを保持
   - 明示的なnullチェックが不要

3. 合成可能性
   - 関数合成と自然に組み合わせられる
   - 複雑な処理を小さな関数の組み合わせで表現

4. TypeScriptでの近似:
   Optional Chaining (?.): obj?.prop?.method()
   これはMaybeのfmapに似た概念
-}

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次の結果を予想してください
q1 :: Maybe String
q1 = fmap show (Just 42)
-- 答え: ???

-- Q2: 次の結果を予想してください
q2 :: [String]
q2 = fmap reverse ["hello", "world"]
-- 答え: ???

-- Q3: なぜこれはコンパイルエラーになるか考えてください
-- q3 = fmap (+1) (Left "error" :: Either String String)
-- ヒント: Either a b の b の型に注目

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Functor Examples ==="

  putStrLn "\n--- List ---"
  print $ fmap (*2) [1, 2, 3, 4, 5]

  putStrLn "\n--- Maybe ---"
  print $ fmap (*2) (Just 5)
  print $ fmap (*2) (Nothing :: Maybe Int)

  putStrLn "\n--- Either ---"
  print $ fmap (*2) (Right 5 :: Either String Int)
  print $ fmap (*2) (Left "error" :: Either String Int)

  putStrLn "\n--- <$> operator ---"
  print $ (*2) <$> [1, 2, 3]

  putStrLn "\n--- 練習問題の答え ---"
  print q1  -- Just "42"
  print q2  -- ["olleh", "dlrow"]
