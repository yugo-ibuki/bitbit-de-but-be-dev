{-
  06-io-monad.hs - IO モナドの理解

  IOモナドはHaskellの「副作用」を扱う方法です。
  純粋な関数型言語で外界とやり取りするための仕組み。
-}

-- ============================================================
-- なぜIOモナドが必要か？
-- ============================================================

{-
関数型プログラミングの原則:
1. 同じ入力には同じ出力（参照透過性）
2. 副作用を持たない

しかし現実のプログラムには副作用が必要:
- ファイルの読み書き
- ユーザー入力の取得
- ネットワーク通信
- 乱数生成
- 現在時刻の取得

解決策: IOモナドで「副作用を持つ計算」を型レベルで区別する

IO a = 「実行すると副作用を起こし、型aの値を返す計算」

重要: IO a は「aの値」ではなく「aを生む計算（レシピ）」
-}

-- ============================================================
-- IO型の理解
-- ============================================================

{-
-- 概念的な定義（実際の実装は異なる）
newtype IO a = IO (RealWorld -> (RealWorld, a))

IO a は:
- 「現実世界の状態」を受け取り
- 「変更された現実世界の状態」と「結果a」を返す関数

これにより:
- 副作用は型システムで追跡される
- 純粋な関数とIOを型で区別できる
- 副作用の順序が保証される
-}

-- ============================================================
-- 基本的なIO操作
-- ============================================================

-- putStrLn :: String -> IO ()
-- 文字列を出力し、何も返さない（()はunitを表す）

-- getLine :: IO String
-- 1行読み込み、文字列を返す

-- print :: Show a => a -> IO ()
-- 任意のShowインスタンスを出力

-- readFile :: FilePath -> IO String
-- ファイルを読み込む

-- writeFile :: FilePath -> String -> IO ()
-- ファイルに書き込む

-- ============================================================
-- IOの連鎖（>>= を使用）
-- ============================================================

-- ユーザーから名前を取得して挨拶
greet :: IO ()
greet =
  putStrLn "What's your name?" >>
  getLine >>= \name ->
  putStrLn ("Hello, " ++ name ++ "!")

-- 分解:
-- 1. putStrLn "What's your name?" :: IO ()
-- 2. >> でシーケンス（結果を捨てる）
-- 3. getLine :: IO String
-- 4. >>= で結果(name)を取り出して次に渡す
-- 5. putStrLn ("Hello, " ++ name ++ "!") :: IO ()

-- ============================================================
-- 他言語との比較
-- ============================================================

{-
【TypeScript / JavaScript】
async/await:

  async function greet() {
    console.log("What's your name?");
    const name = await readline();
    console.log(`Hello, ${name}!`);
  }

Haskell:
  greet = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

共通点:
- 非同期/副作用のある処理を順序付け
- 前の結果を後の処理で使用

違い:
- TypeScriptのasyncは「非同期」のみを示す
- HaskellのIOは「あらゆる副作用」を示す
- Haskellは型レベルで強制（IOを逃れることはできない）

【Rust】
Rustには直接のIOモナドはないが、?演算子でResult/Optionを連鎖:

  fn main() -> io::Result<()> {
      let mut input = String::new();
      println!("What's your name?");
      io::stdin().read_line(&mut input)?;
      println!("Hello, {}!", input.trim());
      Ok(())
  }
-}

-- ============================================================
-- return: 純粋な値をIOに持ち上げる
-- ============================================================

-- return :: a -> IO a
-- 副作用なしで値をIOに入れる

pureInIO :: IO Int
pureInIO = return 42
-- 実行しても何も起きない、ただ42を返す

-- 使い所: IOの連鎖の中で純粋な値を返したいとき
addOne :: IO Int
addOne =
  getLine >>= \s ->
  return (read s + 1)  -- 文字列を数値に変換し、1を足してIOで返す

-- ============================================================
-- IOは「汚染」する
-- ============================================================

{-
重要な概念: IOは一度入ると抜けられない

純粋な関数: String -> Int
IO関数:     String -> IO Int

これらを混ぜることはできない（型が違う）

純粋 → IO: 可能（returnを使う）
IO → 純粋: 不可能（unsafePerformIOを除く、使ってはいけない）

これにより:
- 副作用のある部分が型で明示される
- 純粋な関数のテストが容易
- 参照透過性が保証される領域が明確
-}

-- ============================================================
-- 実例: 簡単なファイル処理
-- ============================================================

-- ファイルを読んで行数を数える
countLines :: FilePath -> IO Int
countLines path =
  readFile path >>= \content ->
  return (length (lines content))

-- ファイルの内容を大文字に変換して書き出す
toUpperFile :: FilePath -> FilePath -> IO ()
toUpperFile input output =
  readFile input >>= \content ->
  writeFile output (map toUpper content)
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c

-- ============================================================
-- 複数のIO操作を組み合わせる
-- ============================================================

-- sequenceとmapM
-- sequence :: [IO a] -> IO [a]
-- リストのIOをまとめて実行

printNumbers :: IO [()]
printNumbers = sequence [print 1, print 2, print 3]
-- 1, 2, 3を順に出力

-- mapM :: (a -> IO b) -> [a] -> IO [b]
-- リストの各要素にIO関数を適用

printAll :: [Int] -> IO ()
printAll xs = mapM_ print xs
-- mapM_ は結果を捨てるバージョン

-- forM: mapMの引数逆順（読みやすいことがある）
import Control.Monad (forM_, replicateM)

printAll' :: [Int] -> IO ()
printAll' xs = forM_ xs print

-- replicateM: 同じIOをn回実行
readThreeLines :: IO [String]
readThreeLines = replicateM 3 getLine

-- ============================================================
-- IOの中での条件分岐
-- ============================================================

-- when: 条件が真のときだけ実行
import Control.Monad (when, unless)

greetIfNotEmpty :: IO ()
greetIfNotEmpty = do
  putStrLn "Enter your name (or leave empty):"
  name <- getLine
  when (not (null name)) $ do
    putStrLn ("Hello, " ++ name ++ "!")

-- unless: 条件が偽のときだけ実行
warnIfEmpty :: String -> IO ()
warnIfEmpty s = unless (not (null s)) $ putStrLn "Warning: empty input"

-- ============================================================
-- IOとMaybe/Eitherの組み合わせ
-- ============================================================

-- 安全なファイル読み込み（存在しないかもしれない）
import System.Directory (doesFileExist)

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
  exists <- doesFileExist path
  if exists
    then fmap Just (readFile path)
    else return Nothing

-- より複雑な例: 設定ファイルのパース
data Config = Config { configPort :: Int, configHost :: String }
  deriving (Show)

-- 簡易的な設定パース（実際はより堅牢に）
parseConfig :: String -> Maybe Config
parseConfig content =
  case lines content of
    [portLine, hostLine] ->
      case (reads (drop 5 portLine), drop 5 hostLine) of
        ([(port, "")], host) | not (null host) -> Just (Config port host)
        _ -> Nothing
    _ -> Nothing

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = do
  mContent <- safeReadFile path
  return (mContent >>= parseConfig)

-- ============================================================
-- IOの本質: 「計算の記述」
-- ============================================================

{-
重要な概念の再確認:

IO a は値ではなく「計算の記述（レシピ）」

例:
  getLine :: IO String
  これは「文字列」ではなく「文字列を読み込む計算」

  putStrLn "Hello" :: IO ()
  これは「何も返さない」ではなく「Helloを出力する計算」

main関数がIO ()なのは:
- プログラム全体が「副作用を起こす計算」
- Haskellランタイムがmainを「実行」する

この設計により:
- 副作用は型で追跡される
- テスト可能性が高まる
- 推論が容易になる
-}

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次のIOアクションを実装してください
-- ユーザーに2つの数を入力させ、その和を出力する
addTwoNumbers :: IO ()
addTwoNumbers = undefined
-- ヒント: getLine, read, print を使う

-- Q2: 次のIOアクションを実装してください
-- 指定された回数だけユーザーに入力を促し、すべての入力をリストで返す
readNLines :: Int -> IO [String]
readNLines n = undefined
-- ヒント: replicateM を使う

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== IO Monad Examples ==="

  putStrLn "\n--- Basic input/output ---"
  greet  -- ユーザー入力を待つ

  putStrLn "\n--- Sequence of IOs ---"
  printAll [1, 2, 3]

  putStrLn "\n--- return in IO ---"
  result <- pureInIO
  print result

  putStrLn "\n--- Conditional IO ---"
  greetIfNotEmpty

  putStrLn "\n--- 練習問題の実装例 ---"
  -- Q1
  putStrLn "Enter two numbers to add:"
  let addTwoNumbers' = do
        putStrLn "First number:"
        n1 <- fmap read getLine :: IO Int
        putStrLn "Second number:"
        n2 <- fmap read getLine :: IO Int
        putStrLn ("Sum: " ++ show (n1 + n2))
  addTwoNumbers'

  -- Q2
  putStrLn "\nEnter 2 lines:"
  let readNLines' n = replicateM n getLine
  inputs <- readNLines' 2
  putStrLn ("You entered: " ++ show inputs)
