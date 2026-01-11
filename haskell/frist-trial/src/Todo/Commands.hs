{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Commands
Description : Todo CLIのコマンド実装（純粋関数のみ）

============================================================
                    学習ポイント
============================================================

【高階関数 (Higher-Order Function)】
関数を引数に取る、または関数を返す関数のこと。
filter, map, find などがその例。

例: filter :: (a -> Bool) -> [a] -> [a]
    ↑ 「a -> Bool」という関数を受け取る

【パターンマッチング】
データの構造を分解して、異なるケースを処理する方法。
if-else チェーンより強力で安全。

【部分適用（Partial Application）】
Haskellの関数はすべてカリー化されている（1つずつ引数を取る）。
例: filter (\t -> todoPriority t == High) todos
    この\t -> ... の部分は各要素に適用される

【純粋関数とは？】
このモジュールの関数はすべて「純粋」:
- 同じ入力 → 必ず同じ出力
- 副作用なし（ファイル操作、画面出力なし）
- テストが簡単！モックも不要！
-}
module Todo.Commands
    ( addTodo
    , listTodos
    , completeTodo
    , deleteTodo
    , filterByPriority
    , filterByStatus
    , nextId
    ) where

import Data.List (find)
import Data.Text (Text)
import Data.Time (UTCTime)

import Todo.Types

{- |
次に使えるIDを生成する。

【最も基本的なパターンマッチ: 空リストの処理】
@
nextId []    = 1              -- 空リストなら1
nextId todos = 1 + maximum ...  -- それ以外
@

【map と maximum の組み合わせ】
1. map todoId todos  → 全TodoからIDだけを取り出す [1, 2, 3]
2. maximum [1, 2, 3] → 最大値 3
3. 1 + 3             → 次のID: 4
-}
nextId :: TodoList -> TodoId
nextId [] = 1
nextId todos = 1 + maximum (map todoId todos)
-- maximum :: Ord a => [a] -> a  （空リストでエラーになる！）
-- map :: (a -> b) -> [a] -> [b]

{- |
新しいTodoを追加する。

【関数の型シグネチャを読む】
addTodo :: Text -> Maybe Text -> Priority -> UTCTime -> TodoList -> TodoList
         ↑タイトル ↑説明(任意)   ↑優先度    ↑時刻      ↑既存リスト  ↑結果

引数が5つある関数。Haskellでは -> で繋がっている。
実は「1つ引数を取って、4引数関数を返す」関数。
(カリー化という概念。最初は気にしなくてOK)

【let ... in 構文】
ローカル変数を定義する。
let 変数 = 値
in  変数を使った式

【レコードの作成】
TodoItem { フィールド1 = 値1, フィールド2 = 値2, ... }

【リストへの追加: (:) コンス演算子】
newTodo : todos は「newTodoを先頭に追加したリスト」
これはO(1)の操作（とても高速）！
-}
addTodo :: Text -> Maybe Text -> Priority -> UTCTime -> TodoList -> TodoList
addTodo title desc priority now todos =
    let newTodo = TodoItem
            { todoId = nextId todos
            , todoTitle = title
            , todoDesc = desc
            , todoPriority = priority
            , todoStatus = Pending
            , todoCreatedAt = now
            }
    -- (:) は「コンス」と読む。リストの先頭に追加。
    -- [1, 2, 3] は 1 : 2 : 3 : [] と同じ
    in newTodo : todos

{- |
全Todoを取得する（そのまま返す）。

【id関数とは？】
id :: a -> a
何もせず、受け取った値をそのまま返す。
「何もしない」ことを明示的に表現できる。

【なぜこの関数があるか？】
関数型プログラミングでは「データ変換」と「副作用（表示）」を分離する。
この関数は「変換」部分。表示はMain.hsで行う。
-}
listTodos :: TodoList -> TodoList
listTodos = id  -- id :: a -> a （恒等関数）

{- |
指定IDのTodoを完了にする。

【mapによる変換】
map :: (a -> b) -> [a] -> [b]
リストの各要素に関数を適用して、新しいリストを作る。

例: map (*2) [1, 2, 3] = [2, 4, 6]

ここでは「IDが一致したら更新、それ以外はそのまま」という
関数を全要素に適用している。

【ガード（|）】
パターンマッチの中で条件分岐する構文。
上から順に評価され、最初にTrueになったものが実行される。

updateIfMatch todo
    | todoId todo == targetId = ...  -- 条件1
    | otherwise               = ...  -- それ以外（elseに相当）

【レコード更新構文】
todo { todoStatus = Completed }
→ todoの todoStatus だけを Completed に変えた「新しい」レコード
→ 元のtodoは変更されない（不変性）！
-}
completeTodo :: TodoId -> TodoList -> TodoList
completeTodo targetId = map updateIfMatch
  where
    -- where節: 関数内でローカル関数を定義
    updateIfMatch todo
        | todoId todo == targetId = todo { todoStatus = Completed }
        | otherwise               = todo

{- |
指定IDのTodoを削除する。

【filter関数】
filter :: (a -> Bool) -> [a] -> [a]
条件を満たす要素だけを残す。

例: filter even [1, 2, 3, 4, 5] = [2, 4]
    (even は偶数ならTrueを返す関数)

【ラムダ式（無名関数）】
\todo -> todoId todo /= targetId
↑バックスラッシュはλ（ラムダ）を表す

\引数 -> 処理
は
(\引数 -> 処理)
と書くことも多い

【比較演算子】
==  等しい
/=  等しくない（!=ではない！）
<, >, <=, >=  大小比較
-}
deleteTodo :: TodoId -> TodoList -> TodoList
deleteTodo targetId = filter (\todo -> todoId todo /= targetId)

{- |
優先度でフィルタリングする。

【部分適用の例】
filterByPriority High todos
は
filter (\todo -> todoPriority todo == High) todos
と同じ。

filterByPriority High
とだけ書くと「Highでフィルタする関数」が得られる！
これを部分適用という。
-}
filterByPriority :: Priority -> TodoList -> TodoList
filterByPriority priority = filter (\todo -> todoPriority todo == priority)

-- | 状態でフィルタリング
filterByStatus :: Status -> TodoList -> TodoList
filterByStatus status = filter (\todo -> todoStatus todo == status)

{- |
指定IDのTodoを探す。

【Maybe型の復習】
Maybe a = Nothing | Just a

- Nothing: 見つからなかった
- Just todo: 見つかった（todoはTodoItem）

【find関数】
find :: (a -> Bool) -> [a] -> Maybe a
条件を満たす最初の要素を返す。
見つからなければNothing、見つかればJust 要素。

【nullポインタがない世界】
他言語: User findUser(id) → nullかもしれない（実行時エラーの元）
Haskell: find :: ... -> Maybe a → 必ずNothingかJustか確認が必要
→ NullPointerException が起きない！
-}
findTodo :: TodoId -> TodoList -> Maybe TodoItem
findTodo targetId = find (\todo -> todoId todo == targetId)

{-
============================================================
                    練習問題
============================================================

1. 【簡単】 優先度をHighに変更する関数を作ってみましょう
   changePriority :: TodoId -> Priority -> TodoList -> TodoList

2. 【中級】 タイトルで検索する関数を作ってみましょう
   searchByTitle :: Text -> TodoList -> TodoList
   ヒント: Data.Text の isInfixOf を使う

3. 【中級】 完了したタスクだけを削除する関数を作ってみましょう
   clearCompleted :: TodoList -> TodoList

4. 【発展】 タスクを優先度でソートする関数を作ってみましょう
   sortByPriority :: TodoList -> TodoList
   ヒント: Data.List の sortBy と comparing を使う

5. REPLで試してみましょう:
   $ cabal repl
   > import Todo.Types
   > import Todo.Commands
   > import Data.Time
   > now <- getCurrentTime
   > let todos = addTodo "Learn Haskell" Nothing High now []
   > todos
   > filterByPriority High todos
   > completeTodo 1 todos
-}
