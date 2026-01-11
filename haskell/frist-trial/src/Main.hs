{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Todo CLIのエントリーポイント

============================================================
                    学習ポイント
============================================================

【main関数】
main :: IO ()
Haskellプログラムのエントリーポイント。
必ず IO () 型（副作用あり、戻り値なし）。

【case式】
パターンマッチを式として使う。
他言語のswitch文に似ているが、より強力：
- 網羅性チェック（全パターンを処理しているか確認）
- パターンで値を分解できる

【このファイルの役割】
- ユーザーからの入力を受け取る
- 他のモジュールの純粋関数を呼び出す
- 結果を画面に表示する

関数型プログラミングでは、副作用を一箇所（ここ）に集める。
-}
module Main where

import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Todo.Types
import Todo.Storage
import Todo.Commands

{- |
プログラムのエントリーポイント。

【コマンドライン引数の処理】
getArgs :: IO [String]
コマンドライン引数をリストで取得。

例: todo-cli add → ["add"]
    todo-cli complete 1 → ["complete", "1"]

【case式でのパターンマッチ】
@
case args of
    ["add"]      -> ...  -- 引数が "add" 1つだけ
    ["list"]     -> ...  -- 引数が "list" 1つだけ
    ["complete", idStr] -> ...  -- 2つの引数をマッチ
    _            -> ...  -- それ以外（ワイルドカード）
@

リストのパターンマッチ:
- []        空リスト
- [x]       要素1つ
- [x, y]    要素2つ
- (x:xs)    先頭とそれ以外
-}
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["add"]             -> runAdd
        ["list"]            -> runList
        ["complete", idStr] -> runComplete (read idStr)
        ["delete", idStr]   -> runDelete (read idStr)
        ["help"]            -> printHelp
        _                   -> printHelp
    -- read :: Read a => String -> a
    -- 文字列を他の型に変換。"123" -> 123

{- |
全Todoを表示する。

【do記法の復習】
@
do
    result <- loadTodos ...  -- IO (Either ...) から Either を取り出す
    case result of           -- Either をパターンマッチ
        Left err -> ...      -- エラーの場合
        Right todos -> ...   -- 成功の場合
@

【mapM_ 関数】
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

リストの各要素にモナド関数を適用し、結果は捨てる。
普通のmapと違い、副作用のために使う。

mapM_ print [1, 2, 3]
→ 1を表示、2を表示、3を表示（戻り値は()）
-}
runList :: IO ()
runList = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "エラー: " ++ err
        Right todos -> mapM_ printTodo todos
  where
    -- where節でローカル関数を定義
    printTodo todo = putStrLn $ formatTodo todo

{- |
Todoを見やすい形式に整形する。

【文字列の連結】
Haskellには文字列補間（"Hello, {name}"）がない。
代わりに連結演算子を使う：

(++) :: [a] -> [a] -> [a]  -- リスト（文字列）の連結

【unwords関数】
unwords :: [String] -> String
文字列のリストをスペースで連結。

unwords ["hello", "world"] = "hello world"
-}
formatTodo :: TodoItem -> String
formatTodo todo = unwords
    [ "[" ++ show (todoId todo) ++ "]"
    , "[" ++ statusToJapanese (todoStatus todo) ++ "]"
    , "[" ++ priorityToJapanese (todoPriority todo) ++ "]"
    , show (todoTitle todo)
    ]

-- | 状態を日本語に変換
statusToJapanese :: Status -> String
statusToJapanese Pending    = "未着手"
statusToJapanese InProgress = "進行中"
statusToJapanese Completed  = "完了"

-- | 優先度を日本語に変換
priorityToJapanese :: Priority -> String
priorityToJapanese High   = "高"
priorityToJapanese Medium = "中"
priorityToJapanese Low    = "低"

{- |
対話形式でTodoを追加する。

【hFlush stdout の理由】
Haskellの出力はバッファリングされている。
putStr の後すぐに表示させるには hFlush が必要。
（putStrLn は改行時に自動フラッシュされる）

【if-then-else式】
Haskellの if は「式」なので、必ず値を返す。
else は省略できない！

let desc = if null input then Nothing else Just (pack input)
             ↑条件          ↑真の場合    ↑偽の場合
-}
runAdd :: IO ()
runAdd = do
    putStr "タイトル: "
    hFlush stdout
    title <- getLine

    putStr "説明（省略可、Enterでスキップ）: "
    hFlush stdout
    descInput <- getLine
    -- null :: [a] -> Bool  -- リストが空かどうか
    -- pack :: String -> Text  -- StringをTextに変換
    let desc = if null descInput then Nothing else Just (pack descInput)

    putStr "優先度（high/medium/low）: "
    hFlush stdout
    priorityInput <- getLine
    let priority = parsePriority priorityInput

    now <- getCurrentTime
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "エラー: " ++ err
        Right todos -> do
            let newTodos = addTodo (pack title) desc priority now todos
            saveTodos defaultFilePath newTodos
            putStrLn "Todoを追加しました！"

{- |
ユーザー入力から優先度を解析する。

【網羅的パターンマッチ】
最後の _ は「それ以外すべて」を捕捉する。
これがないと、想定外の入力でプログラムがクラッシュする。

【デフォルト値】
未知の入力にはMediumを返す。
より堅牢にするなら Maybe Priority を返して
呼び出し側でエラー処理させる方法もある。
-}
parsePriority :: String -> Priority
parsePriority input = case input of
    "high"   -> High
    "h"      -> High
    "高"     -> High
    "medium" -> Medium
    "m"      -> Medium
    "中"     -> Medium
    "low"    -> Low
    "l"      -> Low
    "低"     -> Low
    _        -> Medium  -- デフォルトは中優先度

-- | 指定IDのTodoを完了にする
runComplete :: TodoId -> IO ()
runComplete targetId = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "エラー: " ++ err
        Right todos -> do
            let updatedTodos = completeTodo targetId todos
            saveTodos defaultFilePath updatedTodos
            putStrLn $ "Todo #" ++ show targetId ++ " を完了にしました"

-- | 指定IDのTodoを削除する
runDelete :: TodoId -> IO ()
runDelete targetId = do
    result <- loadTodos defaultFilePath
    case result of
        Left err -> putStrLn $ "エラー: " ++ err
        Right todos -> do
            let updatedTodos = deleteTodo targetId todos
            saveTodos defaultFilePath updatedTodos
            putStrLn $ "Todo #" ++ show targetId ++ " を削除しました"

{- |
ヘルプメッセージを表示する。

【unlines関数】
unlines :: [String] -> String
文字列のリストを改行で連結。

unlines ["a", "b", "c"] = "a\nb\nc\n"
-}
printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "Todo CLI - Haskell学習プロジェクト"
    , ""
    , "使い方:"
    , "  todo-cli add              新しいTodoを追加（対話形式）"
    , "  todo-cli list             全Todoを表示"
    , "  todo-cli complete <id>    指定IDを完了にする"
    , "  todo-cli delete <id>      指定IDを削除"
    , "  todo-cli help             このヘルプを表示"
    ]

{-
============================================================
                    練習問題
============================================================

1. 【簡単】 "進行中"に変更するコマンドを追加してみましょう
   todo-cli start <id>

2. 【中級】 フィルタ機能を追加してみましょう
   todo-cli list --completed
   todo-cli list --high

3. 【中級】 入力検証を追加してみましょう
   - IDが数字でない場合のエラー処理
   - 存在しないIDの場合の警告

4. 【発展】 複数のTodoを一度に追加できるようにしてみましょう
   todo-cli add-multi
   → 空行が入力されるまで連続追加

5. 【発展】 エクスポート機能を追加してみましょう
   todo-cli export markdown > todos.md
   ヒント: TodoItem -> String の関数を作る

6. REPLで試してみましょう:
   $ cabal repl
   > :main add
   > :main list
   > :main complete 1
-}
