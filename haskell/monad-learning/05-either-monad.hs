{-
  05-either-monad.hs - Either モナドの深掘り

  Eitherは「エラー情報を持った失敗」を表すモナドです。
  Maybeと違い、なぜ失敗したかの情報を保持できます。
-}

-- ============================================================
-- Either型の復習
-- ============================================================

{-
data Either a b = Left a | Right b

- Right b: 成功（正しい値 = Right）
- Left a: 失敗（エラー情報を含む）

命名の由来:
- "Right" は「正しい」のダブルミーニング
- 慣習的にRightが成功、Leftが失敗

TypeScriptで言えば:
  type Either<E, A> = { tag: 'Left', error: E } | { tag: 'Right', value: A }
-}

-- ============================================================
-- Either の Monad インスタンス
-- ============================================================

{-
instance Monad (Either e) where
  Left e  >>= _ = Left e     -- エラーはそのまま伝播
  Right a >>= f = f a        -- 成功なら関数を適用

MaybeとEitherの比較:
- Maybe: 失敗 → Nothing（理由不明）
- Either: 失敗 → Left errorInfo（理由あり）
-}

-- ============================================================
-- 基本的な使い方
-- ============================================================

-- 型エイリアスでエラー型を明示
type Error = String
type Result a = Either Error a

-- 安全な除算（エラーメッセージ付き）
safeDiv :: Int -> Int -> Result Int
safeDiv _ 0 = Left "Division by zero"
safeDiv x y = Right (x `div` y)

-- 安全な平方根
safeSqrt :: Double -> Result Double
safeSqrt x
  | x < 0     = Left $ "Cannot take square root of negative number: " ++ show x
  | otherwise = Right (sqrt x)

-- 使用例
divResult1 :: Result Int
divResult1 = safeDiv 10 2
-- 結果: Right 5

divResult2 :: Result Int
divResult2 = safeDiv 10 0
-- 結果: Left "Division by zero"

-- ============================================================
-- 計算の連鎖
-- ============================================================

-- 複数の計算を連鎖（どこかで失敗したらそのエラーが伝播）
calculate :: Int -> Int -> Int -> Result Int
calculate a b c =
  safeDiv a b >>= \ab ->
  safeDiv ab c
-- aをbで割り、その結果をcで割る

calcResult1 :: Result Int
calcResult1 = calculate 100 5 2
-- 結果: Right 10

calcResult2 :: Result Int
calcResult2 = calculate 100 0 2
-- 結果: Left "Division by zero"

calcResult3 :: Result Int
calcResult3 = calculate 100 5 0
-- 結果: Left "Division by zero"

-- ============================================================
-- TypeScript/Rustとの比較
-- ============================================================

{-
【TypeScript】
try-catchまたはResult型を自作:

  type Result<T, E> = { ok: true, value: T } | { ok: false, error: E };

  function safeDiv(a: number, b: number): Result<number, string> {
    if (b === 0) return { ok: false, error: "Division by zero" };
    return { ok: true, value: a / b };
  }

  // チェーンは手動
  const result = safeDiv(10, 2);
  if (!result.ok) return result;
  const next = safeDiv(result.value, 3);
  // ...

【Rust】
Result型が標準で組み込み:

  fn safe_div(a: i32, b: i32) -> Result<i32, String> {
      if b == 0 { return Err("Division by zero".to_string()); }
      Ok(a / b)
  }

  // ?演算子でチェーン
  fn calculate(a: i32, b: i32, c: i32) -> Result<i32, String> {
      let ab = safe_div(a, b)?;
      safe_div(ab, c)
  }

Rustの ? 演算子はHaskellの >>= に似ている！
-}

-- ============================================================
-- 実践例: ユーザー登録バリデーション
-- ============================================================

-- エラー型を定義
data ValidationError
  = EmptyName
  | InvalidAge Int
  | InvalidEmail String
  | PasswordTooShort Int
  deriving (Show, Eq)

type Validation a = Either ValidationError a

-- 各バリデーション関数
validateName :: String -> Validation String
validateName "" = Left EmptyName
validateName name = Right name

validateAge :: Int -> Validation Int
validateAge age
  | age < 0 || age > 150 = Left (InvalidAge age)
  | otherwise = Right age

validateEmail :: String -> Validation String
validateEmail email
  | '@' `notElem` email = Left (InvalidEmail email)
  | otherwise = Right email

validatePassword :: String -> Validation String
validatePassword pwd
  | length pwd < 8 = Left (PasswordTooShort (length pwd))
  | otherwise = Right pwd

-- ユーザー型
data User = User
  { userName :: String
  , userAge :: Int
  , userEmail :: String
  , userPassword :: String
  }
  deriving (Show)

-- すべてのバリデーションを通過してUserを作成
registerUser :: String -> Int -> String -> String -> Validation User
registerUser name age email password =
  validateName name >>= \validName ->
  validateAge age >>= \validAge ->
  validateEmail email >>= \validEmail ->
  validatePassword password >>= \validPassword ->
  Right $ User validName validAge validEmail validPassword

-- 使用例
user1 :: Validation User
user1 = registerUser "Alice" 30 "alice@example.com" "securepassword"
-- 結果: Right (User {...})

user2 :: Validation User
user2 = registerUser "" 30 "alice@example.com" "password123"
-- 結果: Left EmptyName

user3 :: Validation User
user3 = registerUser "Bob" 200 "bob@example.com" "password123"
-- 結果: Left (InvalidAge 200)

user4 :: Validation User
user4 = registerUser "Charlie" 25 "invalid-email" "password123"
-- 結果: Left (InvalidEmail "invalid-email")

user5 :: Validation User
user5 = registerUser "Dave" 25 "dave@example.com" "short"
-- 結果: Left (PasswordTooShort 5)

-- ============================================================
-- エラーハンドリング関数
-- ============================================================

-- either: パターンマッチの便利関数
-- either :: (a -> c) -> (b -> c) -> Either a b -> c

handleResult :: Validation User -> String
handleResult = either handleError handleSuccess
  where
    handleError EmptyName = "Error: Name cannot be empty"
    handleError (InvalidAge n) = "Error: Age " ++ show n ++ " is invalid"
    handleError (InvalidEmail e) = "Error: Email '" ++ e ++ "' is invalid"
    handleError (PasswordTooShort n) = "Error: Password too short (" ++ show n ++ " chars)"
    handleSuccess user = "Welcome, " ++ userName user ++ "!"

-- 使用例
message1 :: String
message1 = handleResult user1
-- "Welcome, Alice!"

message2 :: String
message2 = handleResult user2
-- "Error: Name cannot be empty"

-- ============================================================
-- fromRight / fromLeft: デフォルト値
-- ============================================================

import Data.Either (fromRight, fromLeft, isLeft, isRight)

-- fromRight :: b -> Either a b -> b
-- fromLeft :: a -> Either a b -> a

defaultResult :: Int
defaultResult = fromRight 0 (safeDiv 10 2)
-- 結果: 5

defaultResult2 :: Int
defaultResult2 = fromRight 0 (safeDiv 10 0)
-- 結果: 0（デフォルト値）

-- ============================================================
-- mapLeft / first: エラーの変換
-- ============================================================

import Data.Bifunctor (first)

-- エラー型を変換
convertError :: Either ValidationError a -> Either String a
convertError = first show
-- ValidtionErrorをStringに変換

converted :: Either String User
converted = convertError user2
-- Left "EmptyName"

-- ============================================================
-- 実践パターン: API呼び出しチェーン
-- ============================================================

-- 擬似的なAPI呼び出し（実際はIO Monadと組み合わせる）
type ApiError = String
type ApiResult a = Either ApiError a

-- 認証
authenticate :: String -> String -> ApiResult String
authenticate "admin" "secret" = Right "token-12345"
authenticate _ _ = Left "Invalid credentials"

-- ユーザー情報取得
getUserInfo :: String -> ApiResult (String, Int)
getUserInfo "token-12345" = Right ("Admin User", 999)
getUserInfo _ = Left "Invalid or expired token"

-- 権限チェック
checkPermission :: Int -> ApiResult Bool
checkPermission userId
  | userId > 100 = Right True
  | otherwise = Left "Permission denied"

-- チェーン全体
performAdminAction :: String -> String -> ApiResult Bool
performAdminAction username password =
  authenticate username password >>= \token ->
  getUserInfo token >>= \(_, userId) ->
  checkPermission userId

-- 使用例
adminResult1 :: ApiResult Bool
adminResult1 = performAdminAction "admin" "secret"
-- Right True

adminResult2 :: ApiResult Bool
adminResult2 = performAdminAction "user" "password"
-- Left "Invalid credentials"

-- ============================================================
-- Either vs Maybe: いつ使い分けるか
-- ============================================================

{-
【Maybe を使う場合】
- 「あるかないか」だけが重要
- 失敗の理由が自明または不要
- 例: リストのhead、辞書の検索

【Either を使う場合】
- 失敗の理由を知りたい
- エラーに応じて異なる処理をしたい
- ユーザーにエラーメッセージを表示したい
- 例: バリデーション、API呼び出し、パース
-}

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次の関数を実装してください
-- 文字列を整数にパースし、それが正の数かチェックする
parsePositive :: String -> Either String Int
parsePositive s = undefined
-- ヒント: reads関数を使ってパースできる

-- Q2: 次の結果を予想してください
q2 :: Either String Int
q2 = safeDiv 20 4 >>= \x -> safeDiv x 0 >>= \y -> Right (y + 1)
-- 答え: ???

-- Q3: エラー型を数値（エラーコード）に変換する関数を書いてください
errorCode :: ValidationError -> Int
errorCode = undefined
-- EmptyName → 1, InvalidAge _ → 2, InvalidEmail _ → 3, PasswordTooShort _ → 4

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Either Monad Deep Dive ==="

  putStrLn "\n--- Basic usage ---"
  print $ safeDiv 10 2
  print $ safeDiv 10 0

  putStrLn "\n--- Chain calculation ---"
  print $ calculate 100 5 2
  print $ calculate 100 0 2

  putStrLn "\n--- User registration ---"
  print user1
  print user2
  print user3
  print user4
  print user5

  putStrLn "\n--- Error handling ---"
  putStrLn $ handleResult user1
  putStrLn $ handleResult user2

  putStrLn "\n--- API chain ---"
  print $ performAdminAction "admin" "secret"
  print $ performAdminAction "user" "password"

  putStrLn "\n--- 練習問題の答え ---"
  -- Q1の答え
  let parsePositive' s = case reads s of
        [(n, "")] | n > 0 -> Right n
        [(n, "")] -> Left $ "Not positive: " ++ show n
        _ -> Left $ "Parse error: " ++ s
  print $ parsePositive' "42"       -- Right 42
  print $ parsePositive' "-5"       -- Left "Not positive: -5"
  print $ parsePositive' "abc"      -- Left "Parse error: abc"

  -- Q2の答え
  print q2  -- Left "Division by zero"

  -- Q3の答え
  let errorCode' EmptyName = 1
      errorCode' (InvalidAge _) = 2
      errorCode' (InvalidEmail _) = 3
      errorCode' (PasswordTooShort _) = 4
  print $ errorCode' EmptyName           -- 1
  print $ errorCode' (InvalidAge 200)    -- 2
