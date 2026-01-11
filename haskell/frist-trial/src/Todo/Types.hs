{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Types
Description : Todoアプリケーションの中核データ型

============================================================
                    学習ポイント
============================================================

【代数的データ型 (ADT: Algebraic Data Type)】

Haskellには2種類のADTがあります：

1. 直和型（Sum Type）: 「どれか1つ」を表す（OR）
   例: Priority = High | Medium | Low
   → Priorityは High か Medium か Low のどれか1つ

2. 直積型（Product Type）: 「すべて持つ」を表す（AND）
   例: TodoItem は id と title と priority と... すべてを持つ

他言語との比較:
- TypeScript: type Priority = "High" | "Medium" | "Low" (Union型)
- Java: enum Priority { HIGH, MEDIUM, LOW }
- Haskell: data Priority = High | Medium | Low

【deriving（導出）】
GHCが型クラスのインスタンスを自動生成します。
- Show: 値を文字列に変換（デバッグ用）
- Eq: 等価比較（==）
- Ord: 順序比較（<, >, <=, >=）
- Generic: Aesonによる自動JSON変換の基盤

【レコード構文】
名前付きフィールドを持つ直積型。
TodoItem { todoId :: Int } と書くと、
自動的に todoId :: TodoItem -> Int という関数が生成されます。
-}
module Todo.Types
    ( -- * 中核の型
      TodoItem(..)
    , Priority(..)
    , Status(..)
      -- * 型エイリアス（コードを読みやすくする）
    , TodoId
    , TodoList
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | 型エイリアス: TodoIdは実際にはIntだが、意味が明確になる
--
-- 【なぜ型エイリアスを使うか？】
-- Int と書くより TodoId と書いた方が
-- 「これはタスクのIDだ」と意図が伝わる
type TodoId = Int

{- |
直和型: Priorityは High, Medium, Low のいずれか1つ

【パターン】
data 型名 = コンストラクタ1 | コンストラクタ2 | コンストラクタ3

【他言語との違い】
- これらは「値」ではなく「コンストラクタ」
- 各コンストラクタは関数として使える
- High :: Priority  (Highという関数がPriority型の値を返す)

【Ord導出の意味】
High > Medium > Low という順序が自動的に定義される
（定義順で決まる：最初が最大）
-}
data Priority
    = High    -- 高優先度（最初なので最大）
    | Medium  -- 中優先度
    | Low     -- 低優先度（最後なので最小）
    deriving (Show, Eq, Ord, Generic)

-- Genericから自動的にJSON変換を導出
-- ToJSON: Haskell値 → JSON
-- FromJSON: JSON → Haskell値
instance ToJSON Priority
instance FromJSON Priority

{- |
タスクの状態を表す直和型

【状態遷移の例】
Pending → InProgress → Completed
       ↘            ↗
         (直接完了も可能)
-}
data Status
    = Pending     -- 未着手
    | InProgress  -- 進行中
    | Completed   -- 完了
    deriving (Show, Eq, Generic)

instance ToJSON Status
instance FromJSON Status

{- |
Todoアイテムを表す直積型（レコード）

【直積型とは？】
すべてのフィールドを「同時に」持つ型。
TodoItemは必ず id AND title AND desc AND ... を持つ。

【レコードの自動生成される関数】
各フィールド定義から、ゲッター関数が自動生成される：
- todoId :: TodoItem -> TodoId
- todoTitle :: TodoItem -> Text
- todoDesc :: TodoItem -> Maybe Text
- ...

【Maybeとは？】
値が「あるかもしれないし、ないかもしれない」ことを表す型。
Maybe Text = Nothing | Just Text

- Nothing: 値がない
- Just "説明文": 値がある

他言語のnullと違い、型システムで明示的に表現される。
→ NullPointerExceptionが起きない！
-}
data TodoItem = TodoItem
    { todoId        :: TodoId       -- タスクの一意識別子
    , todoTitle     :: Text         -- タスクのタイトル（必須）
    , todoDesc      :: Maybe Text   -- 説明（オプション、なくてもOK）
    , todoPriority  :: Priority     -- 優先度
    , todoStatus    :: Status       -- 状態
    , todoCreatedAt :: UTCTime      -- 作成日時
    } deriving (Show, Eq, Generic)

instance ToJSON TodoItem
instance FromJSON TodoItem

-- | Todoリストの型エイリアス
-- [TodoItem] と書くより TodoList の方が意図が明確
type TodoList = [TodoItem]

{-
============================================================
                    練習問題
============================================================

1. 新しい優先度「Urgent（緊急）」を追加してみましょう
   ヒント: data Priority = Urgent | High | ... と定義順を変える

2. 新しい状態「Cancelled（キャンセル）」を追加してみましょう

3. タグ機能を追加してみましょう
   ヒント: todoTags :: [Text] というフィールドを追加

4. REPLで試してみましょう:
   $ cabal repl
   > :t todoId           -- 型を確認
   > :t High             -- コンストラクタの型を確認
   > High == Medium      -- 比較
   > High > Medium       -- 順序比較
-}
