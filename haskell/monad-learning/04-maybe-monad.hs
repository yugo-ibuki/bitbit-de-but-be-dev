{-
  04-maybe-monad.hs - Maybe モナドの深掘り

  Maybeは「値があるかもしれない」を表す最も基本的なモナドです。
  null/undefinedの安全な代替として機能します。
-}

-- ============================================================
-- Maybe型の復習
-- ============================================================

{-
data Maybe a = Nothing | Just a

- Just a: 値が存在する
- Nothing: 値が存在しない

TypeScriptで言えば:
  type Maybe<T> = T | null
  ただしHaskellは型安全（nullチェック忘れがコンパイルエラー）
-}

-- ============================================================
-- MaybeがMonadである意味
-- ============================================================

{-
Maybeのbind (>>=) の実装（概念的）:

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

これが意味すること:
- Nothingに対しては何もせずNothingを返す
- Just xに対してはxを取り出してfに渡す

「失敗するかもしれない計算」の連鎖を表現できる！
-}

-- ============================================================
-- 実例: ネストしたデータへのアクセス
-- ============================================================

-- 会社の組織構造
data Company = Company { ceo :: Maybe Employee }
  deriving (Show)

data Employee = Employee
  { empName :: String
  , empManager :: Maybe Employee
  , empDepartment :: Maybe Department
  }
  deriving (Show)

data Department = Department { deptName :: String, deptBudget :: Int }
  deriving (Show)

-- サンプルデータ
sampleCompany :: Company
sampleCompany = Company
  { ceo = Just Employee
    { empName = "Alice"
    , empManager = Nothing
    , empDepartment = Just Department { deptName = "Executive", deptBudget = 1000000 }
    }
  }

-- CEOの部署の予算を取得（失敗するかもしれない）
getCeoBudget :: Company -> Maybe Int
getCeoBudget company =
  ceo company >>= \c ->
  empDepartment c >>= \dept ->
  return (deptBudget dept)

-- 使用例
budgetResult :: Maybe Int
budgetResult = getCeoBudget sampleCompany
-- 結果: Just 1000000

-- CEOがいない場合
emptyCompany :: Company
emptyCompany = Company { ceo = Nothing }

noBudget :: Maybe Int
noBudget = getCeoBudget emptyCompany
-- 結果: Nothing

-- ============================================================
-- TypeScriptとの比較: Optional Chaining
-- ============================================================

{-
TypeScript (Optional Chaining):
  company?.ceo?.department?.budget

Haskell (Monad):
  ceo company >>= empDepartment >>= \d -> return (deptBudget d)

共通点:
- nullチェックを自動化
- 途中でnullがあれば全体がnullになる

違い:
- TypeScriptは ?.  という特殊構文
- Haskellは汎用的なモナド操作
- Haskellの方が型安全（Maybeの処理を強制される）
-}

-- ============================================================
-- 安全な計算の例
-- ============================================================

-- 安全な除算
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- 安全な平方根（負の数はNG）
safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)

-- 安全なhead（空リストはNG）
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- 組み合わせた計算
calculate :: Int -> Int -> Int -> Maybe Int
calculate a b c =
  safeDiv a b >>= \ab ->   -- a / b
  safeDiv ab c              -- (a / b) / c

-- 使用例
calcResult1 :: Maybe Int
calcResult1 = calculate 100 5 2
-- 結果: Just 10 (100 / 5 / 2)

calcResult2 :: Maybe Int
calcResult2 = calculate 100 0 2
-- 結果: Nothing (ゼロ除算)

-- ============================================================
-- fromMaybe: デフォルト値の指定
-- ============================================================

import Data.Maybe (fromMaybe)

-- fromMaybe :: a -> Maybe a -> a
-- Nothingならデフォルト値、Just aならaを返す

withDefault :: Int
withDefault = fromMaybe 0 (safeDiv 10 3)
-- 結果: 3

withDefault2 :: Int
withDefault2 = fromMaybe 0 (safeDiv 10 0)
-- 結果: 0（デフォルト値）

-- ============================================================
-- maybe: Maybe値の処理
-- ============================================================

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- Nothingならデフォルト、Just aなら関数を適用

processResult :: String
processResult = maybe "No result" show (safeDiv 10 3)
-- 結果: "3"

processResult2 :: String
processResult2 = maybe "No result" show (safeDiv 10 0)
-- 結果: "No result"

-- ============================================================
-- パターンマッチング vs モナド操作
-- ============================================================

-- パターンマッチング（verbose）
getUserAge1 :: Maybe Employee -> Maybe Int
getUserAge1 mEmp = case mEmp of
  Nothing -> Nothing
  Just emp -> case empDepartment emp of
    Nothing -> Nothing
    Just dept -> Just (deptBudget dept)

-- モナド操作（concise）
getUserAge2 :: Maybe Employee -> Maybe Int
getUserAge2 mEmp =
  mEmp >>= empDepartment >>= \d -> return (deptBudget d)

-- パターンマッチングはネストが深くなると読みにくい
-- モナド操作はフラットで読みやすい

-- ============================================================
-- <|> (Alternative): 代替値
-- ============================================================

import Control.Applicative ((<|>))

-- <|> は「最初のJustを返す」演算子
alternative1 :: Maybe Int
alternative1 = Nothing <|> Just 5
-- 結果: Just 5

alternative2 :: Maybe Int
alternative2 = Just 3 <|> Just 5
-- 結果: Just 3（最初のJust）

-- 複数の代替を連鎖
alternative3 :: Maybe Int
alternative3 = Nothing <|> Nothing <|> Just 10 <|> Just 20
-- 結果: Just 10

-- 実用例: 複数の方法で設定を取得
getConfig :: String -> Maybe String
getConfig "A" = Just "ConfigA"
getConfig _ = Nothing

getEnv :: String -> Maybe String
getEnv "B" = Just "EnvB"
getEnv _ = Nothing

getDefault :: String -> Maybe String
getDefault _ = Just "Default"

-- 優先順位: Config → Env → Default
getSetting :: String -> Maybe String
getSetting key = getConfig key <|> getEnv key <|> getDefault key

settingA :: Maybe String
settingA = getSetting "A"  -- Just "ConfigA"

settingB :: Maybe String
settingB = getSetting "B"  -- Just "EnvB"

settingC :: Maybe String
settingC = getSetting "C"  -- Just "Default"

-- ============================================================
-- 実践パターン: バリデーションチェーン
-- ============================================================

type ValidationError = String

-- 検証関数
validateAge :: Int -> Maybe Int
validateAge age
  | age >= 0 && age < 150 = Just age
  | otherwise = Nothing

validateName :: String -> Maybe String
validateName name
  | not (null name) && length name < 100 = Just name
  | otherwise = Nothing

validateEmail :: String -> Maybe String
validateEmail email
  | '@' `elem` email = Just email
  | otherwise = Nothing

-- ユーザー型
data User = User
  { uName :: String
  , uAge :: Int
  , uEmail :: String
  }
  deriving (Show)

-- すべてのバリデーションを通過して初めてUserを作成
createUser :: String -> Int -> String -> Maybe User
createUser name age email =
  validateName name >>= \validName ->
  validateAge age >>= \validAge ->
  validateEmail email >>= \validEmail ->
  return $ User validName validAge validEmail

-- 使用例
user1 :: Maybe User
user1 = createUser "Alice" 30 "alice@example.com"
-- 結果: Just (User {uName = "Alice", uAge = 30, uEmail = "alice@example.com"})

user2 :: Maybe User
user2 = createUser "" 30 "alice@example.com"
-- 結果: Nothing（名前が空）

user3 :: Maybe User
user3 = createUser "Bob" 200 "bob@example.com"
-- 結果: Nothing（年齢が不正）

user4 :: Maybe User
user4 = createUser "Charlie" 25 "invalid-email"
-- 結果: Nothing（メールが不正）

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次の関数を実装してください
-- 2つの数を安全に割り、さらにその結果を安全に割る
divideThrice :: Int -> Int -> Int -> Int -> Maybe Int
divideThrice a b c d = undefined
-- ヒント: safeDiv を 3回連鎖させる

-- Q2: 次の結果を予想してください
q2 :: Maybe Int
q2 = Nothing <|> safeDiv 10 2 <|> safeDiv 10 0
-- 答え: ???

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Maybe Monad Deep Dive ==="

  putStrLn "\n--- Nested data access ---"
  print $ getCeoBudget sampleCompany
  print $ getCeoBudget emptyCompany

  putStrLn "\n--- Safe calculations ---"
  print $ calculate 100 5 2
  print $ calculate 100 0 2

  putStrLn "\n--- fromMaybe ---"
  print $ fromMaybe 0 (safeDiv 10 3)
  print $ fromMaybe 0 (safeDiv 10 0)

  putStrLn "\n--- Alternative (<|>) ---"
  print $ Nothing <|> Just 5
  print $ Just 3 <|> Just 5
  print $ getSetting "A"
  print $ getSetting "C"

  putStrLn "\n--- Validation ---"
  print $ createUser "Alice" 30 "alice@example.com"
  print $ createUser "" 30 "alice@example.com"
  print $ createUser "Bob" 200 "bob@example.com"

  putStrLn "\n--- 練習問題の答え ---"
  -- Q1の答え
  let divideThrice' a b c d = safeDiv a b >>= \ab -> safeDiv ab c >>= \abc -> safeDiv abc d
  print $ divideThrice' 100 2 5 2  -- Just 5
  print $ divideThrice' 100 0 5 2  -- Nothing

  -- Q2の答え
  print q2  -- Just 5
