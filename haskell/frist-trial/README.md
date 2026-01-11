# Todo CLI - Haskell学習プロジェクト

Haskellの基本概念を実践的に学ぶためのTodoアプリケーションです。

## このプロジェクトで学べること

| 概念 | 説明 | 該当ファイル |
|------|------|-------------|
| **代数的データ型 (ADT)** | 直和型・直積型でデータを表現 | Types.hs |
| **パターンマッチ** | データの分解と条件分岐 | Commands.hs, Main.hs |
| **高階関数** | map, filter, find などの活用 | Commands.hs |
| **IOモナド** | 副作用（ファイル操作）の扱い方 | Storage.hs, Main.hs |
| **Maybe / Either** | null/例外を使わないエラー処理 | Commands.hs, Storage.hs |
| **型クラス** | Show, Eq, ToJSON の導出 | Types.hs |
| **レコード構文** | 名前付きフィールドを持つ型 | Types.hs |
| **do記法** | モナド操作の糖衣構文 | Storage.hs, Main.hs |

## セットアップ

### 1. GHCupのインストール

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

インストール後、ターミナルを再起動するか以下を実行:
```bash
source ~/.ghcup/env
```

### 2. ビルドと実行

```bash
# 依存関係のインストールとビルド
cabal build

# 実行
cabal run todo-cli -- help
```

## 使い方

```bash
# Todoを追加（対話形式）
cabal run todo-cli -- add

# 一覧表示
cabal run todo-cli -- list

# 完了にする
cabal run todo-cli -- complete 1

# 削除する
cabal run todo-cli -- delete 1
```

## 学習の進め方

### ステップ1: 型を理解する（Types.hs）

まず `src/Todo/Types.hs` を読んでください。

**重要な概念:**
- `data Priority = High | Medium | Low` → **直和型**（どれか1つ）
- `data TodoItem = TodoItem { ... }` → **直積型**（すべて持つ）
- `Maybe Text` → 値があるかもしれないし、ないかもしれない

**REPLで試す:**
```haskell
$ cabal repl
> :t High           -- Highの型を確認
> High == Medium    -- 比較してみる
> :t todoId         -- 自動生成されたゲッター関数
```

### ステップ2: 純粋関数を理解する（Commands.hs）

次に `src/Todo/Commands.hs` を読んでください。

**重要な概念:**
- `map`, `filter`, `find` などの高階関数
- ラムダ式: `\todo -> todoId todo == 1`
- ガード: `| 条件 = 結果`

**REPLで試す:**
```haskell
$ cabal repl
> import Todo.Types
> import Todo.Commands
> import Data.Time
> now <- getCurrentTime
> let todos = addTodo "Learn Haskell" Nothing High now []
> todos
> filterByPriority High todos
> completeTodo 1 todos
```

### ステップ3: IOモナドを理解する（Storage.hs）

`src/Todo/Storage.hs` を読んでください。

**重要な概念:**
- `IO a` → 副作用を伴う計算
- `<-` → IOから値を取り出す
- `pure` / `return` → 値をIOに包む
- `Either String a` → エラーか成功か

**注意点:**
```haskell
-- これはコンパイルエラー！
badFunction :: FilePath -> TodoList
badFunction path = loadTodos path  -- IO (Either ...) は TodoList ではない

-- 正しくはdo記法の中で使う
goodFunction :: FilePath -> IO TodoList
goodFunction path = do
    result <- loadTodos path
    case result of
        Left _ -> pure []
        Right todos -> pure todos
```

### ステップ4: すべてを組み合わせる（Main.hs）

最後に `src/Main.hs` を読んでください。

**重要な概念:**
- `case`式によるパターンマッチ
- 副作用（画面出力、ユーザー入力）の集約
- `mapM_` によるリストのIO処理

## よくある疑問

### Q: なぜ `return` が関数を終了させないの？

Haskellの `return` は他言語と違います:
```haskell
return :: a -> IO a
-- 値をIOに「包む」だけ。関数は終了しない！

example :: IO ()
example = do
    return ()  -- ここで終了しない
    putStrLn "これは実行される！"
```

### Q: `$` って何？

関数適用演算子。括弧を減らすために使います:
```haskell
-- これらは同じ
putStrLn (show (1 + 2))
putStrLn $ show $ 1 + 2
```

### Q: `<-` と `let` の違いは？

```haskell
do
    x <- getLine      -- IOから値を取り出す（副作用あり）
    let y = length x  -- 純粋な計算（副作用なし）
    ...
```

### Q: なぜ Maybe を使うの？

null/nil がないため、「値がない可能性」を型で表現します:
```haskell
-- 他言語: User findUser(id)  -- nullかも（実行時エラーの可能性）
-- Haskell: findUser :: Id -> Maybe User  -- 必ず Nothing か Just か確認が必要
```

## 練習問題

各ファイルの末尾に練習問題があります。難易度順に:

1. **簡単** - 既存パターンの模倣
2. **中級** - 新しい概念の組み合わせ
3. **発展** - 新しいHaskell概念の学習が必要

## 次のステップ

このプロジェクトを終えたら:

1. **Functor / Applicative / Monad** - 型クラス階層の理解
2. **State モナド** - 状態を扱うパターン
3. **Parser Combinator** - パーサーの作り方
4. **並行処理** - async, STM

おすすめリソース:
- [すごいHaskellたのしく学ぼう！](http://learnyouahaskell.com/)
- [Haskell入門](https://www.shuwasystem.co.jp/book/9784798048062.html)
- [Real World Haskell](http://book.realworldhaskell.org/)
