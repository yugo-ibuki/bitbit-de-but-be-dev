{-
  07-do-notation.hs - do記法の理解

  do記法はモナド操作を読みやすく書くための糖衣構文です。
  内部的には >>= と >> に変換されます。
-}

-- ============================================================
-- do記法の基本
-- ============================================================

{-
do記法は以下のように変換される:

do { x <- action; rest }  →  action >>= \x -> rest
do { action; rest }       →  action >> rest
do { let x = expr; rest } →  let x = expr in rest
do { expr }               →  expr

例:
  do
    x <- getLine
    y <- getLine
    return (x ++ y)

  -- 変換後:
  getLine >>= \x ->
  getLine >>= \y ->
  return (x ++ y)
-}

-- ============================================================
-- 基本例: IOでのdo記法
-- ============================================================

-- do記法なし
greetBind :: IO ()
greetBind =
  putStrLn "What's your name?" >>
  getLine >>= \name ->
  putStrLn ("Hello, " ++ name ++ "!")

-- do記法あり
greetDo :: IO ()
greetDo = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")

-- 両者は完全に等価！

-- ============================================================
-- do記法の構文要素
-- ============================================================

-- 1. <- : モナドから値を取り出す
extractExample :: IO ()
extractExample = do
  line <- getLine        -- IO String から String を取り出す
  putStrLn line

-- 2. let : 純粋な値を束縛（モナドではない）
letExample :: IO ()
letExample = do
  line <- getLine
  let upper = map toUpper line  -- 純粋な計算（<-は不要）
  putStrLn upper
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c

-- 3. 単なる式 : 結果を捨てるアクション
actionExample :: IO ()
actionExample = do
  putStrLn "First"    -- 結果(())を捨てる
  putStrLn "Second"   -- 結果(())を捨てる
  putStrLn "Third"    -- 最後の式が全体の結果

-- 4. return : 値をモナドに包んで返す
returnExample :: IO String
returnExample = do
  name <- getLine
  return ("Hello, " ++ name)  -- IO Stringを返す

-- ============================================================
-- MaybeでのDo記法
-- ============================================================

-- do記法はIOだけでなく、すべてのMonadで使える！

-- Maybe での例（失敗するかもしれない計算の連鎖）
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- do記法なし
calculateBind :: Int -> Int -> Int -> Maybe Int
calculateBind a b c =
  safeDiv a b >>= \ab ->
  safeDiv ab c

-- do記法あり
calculateDo :: Int -> Int -> Int -> Maybe Int
calculateDo a b c = do
  ab <- safeDiv a b    -- Maybe Int から Int を取り出す
  safeDiv ab c         -- 最後の式が結果

-- 使用例
calcResult1 :: Maybe Int
calcResult1 = calculateDo 100 5 2  -- Just 10

calcResult2 :: Maybe Int
calcResult2 = calculateDo 100 0 2  -- Nothing

-- ============================================================
-- EitherでのDo記法
-- ============================================================

type Error = String
type Result a = Either Error a

safeDivE :: Int -> Int -> Result Int
safeDivE _ 0 = Left "Division by zero"
safeDivE x y = Right (x `div` y)

-- do記法での連鎖
calculateE :: Int -> Int -> Int -> Result Int
calculateE a b c = do
  ab <- safeDivE a b
  abc <- safeDivE ab c
  return abc  -- または単に safeDivE ab c でOK

-- エラー情報付きの失敗
calcResultE1 :: Result Int
calcResultE1 = calculateE 100 5 2  -- Right 10

calcResultE2 :: Result Int
calcResultE2 = calculateE 100 0 2  -- Left "Division by zero"

-- ============================================================
-- ListでのDo記法（非決定性計算）
-- ============================================================

-- Listのdo記法は「すべての組み合わせ」を生成

-- 九九の表の一部
multiplicationTable :: [(Int, Int, Int)]
multiplicationTable = do
  x <- [1..3]           -- xは1,2,3のいずれか
  y <- [1..3]           -- yは1,2,3のいずれか
  return (x, y, x * y)  -- すべての組み合わせ
-- 結果: [(1,1,1),(1,2,2),(1,3,3),(2,1,2),(2,2,4),(2,3,6),(3,1,3),(3,2,6),(3,3,9)]

-- ピタゴラス数を見つける
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = do
  a <- [1..n]
  b <- [a..n]         -- bはa以上（重複を避ける）
  c <- [b..n]         -- cはb以上
  if a*a + b*b == c*c
    then return (a, b, c)
    else []           -- 空リスト = 失敗 = この組み合わせを除外

-- 使用例
pythResult :: [(Int, Int, Int)]
pythResult = pythagorean 20
-- 結果: [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]

-- ============================================================
-- guard関数（リストモナドでのフィルタリング）
-- ============================================================

import Control.Monad (guard)

-- guard :: Bool -> m ()
-- 条件が偽ならその分岐を「失敗」させる

pythagoreanWithGuard :: Int -> [(Int, Int, Int)]
pythagoreanWithGuard n = do
  a <- [1..n]
  b <- [a..n]
  c <- [b..n]
  guard (a*a + b*b == c*c)  -- 条件を満たさない組み合わせを除外
  return (a, b, c)

-- 偶数のペアを生成
evenPairs :: [(Int, Int)]
evenPairs = do
  x <- [1..5]
  y <- [1..5]
  guard (even x && even y)
  return (x, y)
-- 結果: [(2,2),(2,4),(4,2),(4,4)]

-- ============================================================
-- do記法の変換規則（詳細）
-- ============================================================

{-
1. 最後の式
   do { e }  →  e

2. バインド
   do { x <- e; stmts }  →  e >>= \x -> do { stmts }

3. シーケンス
   do { e; stmts }  →  e >> do { stmts }

4. let
   do { let decls; stmts }  →  let decls in do { stmts }

例（段階的な変換）:
   do
     x <- getLine
     let y = x ++ "!"
     putStrLn y
     return ()

   -- ステップ1: xのバインド
   getLine >>= \x -> do
     let y = x ++ "!"
     putStrLn y
     return ()

   -- ステップ2: let
   getLine >>= \x ->
     let y = x ++ "!" in do
       putStrLn y
       return ()

   -- ステップ3: シーケンス
   getLine >>= \x ->
     let y = x ++ "!" in
       putStrLn y >> return ()
-}

-- ============================================================
-- よくある間違いと注意点
-- ============================================================

-- 間違い1: 最後の式で <- を使う
-- wrongExample = do
--   x <- getLine
--   result <- return x  -- 不必要な <- 、ただのreturn xでOK

correctExample :: IO String
correctExample = do
  x <- getLine
  return x  -- OK

-- 間違い2: 純粋な値に <- を使う
-- wrongExample2 = do
--   x <- 5  -- 間違い！5はモナドではない
--   return x

correctExample2 :: Maybe Int
correctExample2 = do
  let x = 5  -- 純粋な値は let で
  return x

-- 間違い3: do の中で return しても終了しない
continuesAfterReturn :: IO ()
continuesAfterReturn = do
  putStrLn "Before return"
  return ()           -- これは終了しない！ただ () を返すだけ
  putStrLn "After return"  -- これも実行される

-- 正しく早期リターンするには条件分岐を使う
earlyReturn :: Int -> IO ()
earlyReturn n = do
  if n < 0
    then putStrLn "Negative number, stopping"
    else do
      putStrLn "Processing..."
      putStrLn ("Result: " ++ show (n * 2))

-- ============================================================
-- 実践例: 簡単なREPL
-- ============================================================

-- 簡単な電卓REPL
calculator :: IO ()
calculator = do
  putStrLn "Enter expression (or 'quit' to exit):"
  input <- getLine
  if input == "quit"
    then putStrLn "Goodbye!"
    else do
      let result = eval input
      case result of
        Just n  -> putStrLn ("Result: " ++ show n)
        Nothing -> putStrLn "Invalid expression"
      calculator  -- 再帰でループ

-- 簡易的な評価（実際はパーサーを使う）
eval :: String -> Maybe Int
eval s = case words s of
  [a, "+", b] -> (+) <$> readMaybe a <*> readMaybe b
  [a, "-", b] -> (-) <$> readMaybe a <*> readMaybe b
  [a, "*", b] -> (*) <$> readMaybe a <*> readMaybe b
  _ -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次のコードをdo記法を使わずに書き換えてください
q1Do :: Maybe Int
q1Do = do
  x <- Just 5
  y <- Just 3
  return (x + y)
-- >>= を使って書き換える: ???

-- Q2: 次のコードをdo記法で書き換えてください
q2Bind :: IO ()
q2Bind = getLine >>= \name -> putStrLn name >> putStrLn "Done"
-- do記法で書き換える: ???

-- Q3: 1から10までの数のうち、3の倍数のものをリストで返す
-- do記法とguardを使って実装してください
multiplesOfThree :: [Int]
multiplesOfThree = undefined

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Do Notation Examples ==="

  putStrLn "\n--- Basic IO with do ---"
  greetDo

  putStrLn "\n--- Maybe with do ---"
  print $ calculateDo 100 5 2
  print $ calculateDo 100 0 2

  putStrLn "\n--- Either with do ---"
  print $ calculateE 100 5 2
  print $ calculateE 100 0 2

  putStrLn "\n--- List with do ---"
  print multiplicationTable
  print $ pythagorean 15

  putStrLn "\n--- Guard ---"
  print evenPairs

  putStrLn "\n--- 練習問題の答え ---"
  -- Q1
  let q1Bind = Just 5 >>= \x -> Just 3 >>= \y -> return (x + y)
  print q1Bind  -- Just 8

  -- Q2
  let q2Do = do
        name <- getLine
        putStrLn name
        putStrLn "Done"
  putStrLn "(Q2 is an IO action)"

  -- Q3
  let multiplesOfThree' = do
        n <- [1..10]
        guard (n `mod` 3 == 0)
        return n
  print multiplesOfThree'  -- [3,6,9]

  -- putStrLn "\n--- Calculator REPL ---"
  -- calculator  -- これはインタラクティブなのでコメントアウト
