{-
  03-monad-basics.hs - Monadの基礎

  Monadは「連鎖可能な計算」を表す型クラスです。
  前の計算の結果に基づいて次の計算を決定できます。
-}

-- ============================================================
-- Monad 型クラスの定義（概念理解用）
-- ============================================================

{-
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b  -- bind (バインド)
  (>>)   :: m a -> m b -> m b         -- sequence (シーケンス)
  return :: a -> m a                   -- pure と同じ

  読み方:
  - >>= (bind): m aを取り、「aを受け取りm bを返す関数」を適用
  - >>: 最初の計算を実行し、結果を捨てて次の計算を実行
  - return: 値を文脈に入れる（pure と同義）

  重要: Monad は Applicative を継承
        Applicative は Functor を継承
        つまり: Functor ⊂ Applicative ⊂ Monad
-}

-- ============================================================
-- 型クラス階層の図解
-- ============================================================

{-
    Functor
       ↑
       │ (fmap: 関数をコンテナ内の値に適用)
       │
   Applicative
       ↑
       │ (pure: 値を文脈に入れる)
       │ (<*>: 文脈内の関数を文脈内の値に適用)
       │
     Monad
       │ (>>=: 文脈内の値を取り出し、新しい文脈を生成する関数に渡す)
       │ (return: 値を文脈に入れる = pure)

各層が追加する能力:
- Functor: 中身を変換できる
- Applicative: 複数の文脈を組み合わせられる（独立した計算）
- Monad: 計算を連鎖させ、前の結果に依存できる（依存した計算）
-}

-- ============================================================
-- Applicative vs Monad の違い
-- ============================================================

{-
【Applicative】独立した計算の組み合わせ
  f <$> mx <*> my
  - mxとmyは互いに依存しない
  - 両方の計算が先に決まっている

【Monad】依存した計算の連鎖
  mx >>= \x -> my x
  - myの計算はxの値に依存する
  - xが分かってからmyが決まる

具体例:
  -- Applicative: ユーザーIDとパスワードの両方をチェック
  validate <$> getUserId <*> getPassword

  -- Monad: ユーザーIDを取得し、そのIDでDBを検索
  getUserId >>= \userId -> lookupUserInDB userId
  -- lookupUserInDBはuserIdがないと実行できない
-}

-- ============================================================
-- >>= (bind) の理解
-- ============================================================

-- Maybe での >>=
bindExample1 :: Maybe Int
bindExample1 = Just 5 >>= \x -> Just (x * 2)
-- 結果: Just 10
-- 分解: Just 5 から 5 を取り出し、Just (5 * 2) を返す

bindExample2 :: Maybe Int
bindExample2 = Nothing >>= \x -> Just (x * 2)
-- 結果: Nothing
-- Nothingには取り出す値がないので、関数は実行されない

-- 連鎖
chainExample :: Maybe Int
chainExample = Just 5 >>= \x ->
               Just (x + 3) >>= \y ->
               Just (y * 2)
-- 結果: Just 16
-- 5 → 8 → 16

-- ============================================================
-- >> (sequence) の理解
-- ============================================================

-- >> は左の結果を捨てて右を実行
seqExample :: Maybe Int
seqExample = Just 5 >> Just 10
-- 結果: Just 10（Just 5 の値は捨てられる）

seqExample2 :: Maybe Int
seqExample2 = Nothing >> Just 10
-- 結果: Nothing（左が失敗したら右は実行されない）

-- ============================================================
-- return の理解
-- ============================================================

-- return は pure と同じ（歴史的理由で両方存在）
returnExample :: Maybe Int
returnExample = return 5
-- 結果: Just 5

-- リストでは
returnList :: [Int]
returnList = return 5
-- 結果: [5]

-- ============================================================
-- 他言語との比較
-- ============================================================

{-
【TypeScript / JavaScript との比較】

Promise チェーン:
  fetch('/api/user')
    .then(response => response.json())
    .then(user => fetch(`/api/posts/${user.id}`))
    .then(response => response.json())

Haskell Monad:
  getUser >>= \user ->
  getPosts (userId user) >>= \posts ->
  return posts

-- または do記法で
  do
    user <- getUser
    posts <- getPosts (userId user)
    return posts

共通点:
- 前の結果を使って次の処理を決定
- エラーは自動的に伝播

違い:
- Haskellは型安全（コンパイル時にエラー検出）
- Promiseは非同期のみ、Monadは任意の文脈

【Rust との比較】

Rust (and_then):
  get_user()
    .and_then(|user| get_posts(user.id))
    .and_then(|posts| Ok(posts))

Haskell:
  getUser >>= \user ->
  getPosts (userId user) >>= \posts ->
  return posts

Rustのand_thenはHaskellの>>= に非常に近い！
-}

-- ============================================================
-- Monad法則
-- ============================================================

{-
1. Left Identity（左単位元）
   return a >>= f  ==  f a
   「returnした値にbindしても、直接関数を適用するのと同じ」

2. Right Identity（右単位元）
   m >>= return  ==  m
   「returnをbindしても元のまま」

3. Associativity（結合法則）
   (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
   「bindの順序を変えても結果は同じ」

これらの法則が保証するもの:
- リファクタリングの安全性
- 予測可能な振る舞い
- do記法の正当性
-}

-- ============================================================
-- 実用例: データ検索チェーン
-- ============================================================

-- 簡易的なデータベース
type UserId = Int
type PostId = Int

data User = User { userName :: String, userAge :: Int }
  deriving (Show)

data Post = Post { postTitle :: String, postAuthor :: UserId }
  deriving (Show)

-- 「データベース」
users :: [(UserId, User)]
users = [(1, User "Alice" 30), (2, User "Bob" 25)]

posts :: [(PostId, Post)]
posts = [(101, Post "Hello World" 1), (102, Post "Haskell is Fun" 1)]

-- 検索関数（失敗するかもしれない）
findUser :: UserId -> Maybe User
findUser uid = lookup uid users

findPost :: PostId -> Maybe Post
findPost pid = lookup pid posts

findUserByPost :: PostId -> Maybe User
findUserByPost pid =
  findPost pid >>= \post ->           -- 投稿を探す
  findUser (postAuthor post)           -- 投稿の著者を探す

-- 使用例
searchResult1 :: Maybe User
searchResult1 = findUserByPost 101
-- 結果: Just (User "Alice" 30)

searchResult2 :: Maybe User
searchResult2 = findUserByPost 999
-- 結果: Nothing（投稿が存在しない）

-- ============================================================
-- >>=の本質: 「文脈を繋ぐ」
-- ============================================================

{-
>>= の型を再確認:
  (>>=) :: m a -> (a -> m b) -> m b

これが意味すること:
1. m a から a を「取り出す」（文脈を考慮して）
2. a を関数に渡す
3. 関数は新しい文脈 m b を返す
4. 全体として m b が得られる

Maybeの場合:
- 値がある(Just a): aを取り出して関数に渡す
- 値がない(Nothing): 関数を呼ばずにNothingを返す

Listの場合:
- 各要素に関数を適用
- 結果のリストを連結（flatMap）

IOの場合:
- 副作用を実行し、結果を次の関数に渡す
-}

-- ============================================================
-- 練習問題
-- ============================================================

-- Q1: 次の結果を予想してください
q1 :: Maybe Int
q1 = Just 10 >>= \x -> Just (x + 5) >>= \y -> Just (y * 2)
-- 答え: ???

-- Q2: 次の結果を予想してください
q2 :: Maybe Int
q2 = Just 10 >>= \_ -> Nothing >>= \y -> Just (y * 2)
-- 答え: ???

-- Q3: 次の式を >>= を使って書き換えてください
-- liftA2 (+) (Just 3) (Just 5)
-- ヒント: Just 3 >>= \a -> ???

-- ============================================================
-- main（実行確認用）
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Monad Basics ==="

  putStrLn "\n--- bind (>>=) ---"
  print $ Just 5 >>= \x -> Just (x * 2)
  print $ (Nothing :: Maybe Int) >>= \x -> Just (x * 2)

  putStrLn "\n--- Chain ---"
  print chainExample

  putStrLn "\n--- sequence (>>) ---"
  print $ Just 5 >> Just 10
  print $ (Nothing :: Maybe Int) >> Just 10

  putStrLn "\n--- Data search chain ---"
  print $ findUserByPost 101
  print $ findUserByPost 999

  putStrLn "\n--- 練習問題の答え ---"
  print q1  -- Just 30
  print q2  -- Nothing
  -- Q3の答え: Just 3 >>= \a -> Just 5 >>= \b -> Just (a + b)
  print $ Just 3 >>= \a -> Just 5 >>= \b -> Just (a + b)
