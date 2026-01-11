{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Todo.Storage
Description : TodoリストのJSON永続化

============================================================
                    学習ポイント
============================================================

【IOモナド - Haskell最大の特徴】

Haskellは「純粋関数型言語」なので、副作用（ファイル操作、画面出力など）を
特別な方法で扱います。それがIOモナドです。

純粋関数: 入力 → 出力 （毎回同じ結果）
IO関数:   入力 → IO 出力 （外部世界とのやり取りあり）

【なぜIOを分けるのか？】
1. 副作用がある関数が一目で分かる（型を見れば分かる）
2. 純粋な部分はテストが簡単（モック不要）
3. コンパイラが副作用を追跡できる

【Either によるエラー処理】
Either Left Right: Leftがエラー、Rightが成功

Either String TodoList の場合:
- Left "ファイルが壊れています" → エラー
- Right [todo1, todo2] → 成功

例外を投げる代わりに、戻り値でエラーを表現する。
→ エラー処理を忘れるとコンパイルエラー！

【do記法】
モナド操作を順番に書くための糖衣構文（シンタックスシュガー）。
命令型プログラミングのように見えるが、実際は関数の連鎖。
-}
module Todo.Storage
    ( loadTodos
    , saveTodos
    , defaultFilePath
    ) where

import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

import Todo.Types (TodoList)

-- | デフォルトの保存先ファイルパス
defaultFilePath :: FilePath
defaultFilePath = "todos.json"

{- |
JSONファイルからTodoリストを読み込む。

【IOモナドの基本構造】

@
loadTodos :: FilePath -> IO (Either String TodoList)
                         ↑   ↑
                         │   └─ 「Either String TodoList」という値が入っている
                         └───── IOという「箱」
@

IOは「副作用を伴う計算」を表す箱。
箱の中身は、実行されるまで取り出せない。

【do記法の解説】

@
do
    exists <- doesFileExist path  -- IO Bool から Bool を取り出す
    if exists
        then ...
        else return []            -- [] を IO に包む
@

<- は「箱から取り出す」操作。
return は「値を箱に入れる」操作（他言語のreturnとは違う！）。

【Haskellの return は特殊】
return :: a -> m a
値をモナド（ここではIO）に「包む」だけ。
関数を終了させる意味はない！

例: return 5 :: IO Int  -- 5をIOに包んだもの

【pure と return】
pure :: Applicative f => a -> f a
return :: Monad m => a -> m a

実質同じ。pureの方がモダンで推奨。
-}
loadTodos :: FilePath -> IO (Either String TodoList)
loadTodos path = do
    -- doesFileExist :: FilePath -> IO Bool
    -- ファイルが存在するかチェック（副作用：ファイルシステムアクセス）
    exists <- doesFileExist path

    if exists
        then do
            -- BS.readFile :: FilePath -> IO ByteString
            -- ファイルを読み込む（副作用：ファイル読み込み）
            contents <- BS.readFile path

            -- eitherDecodeStrict :: FromJSON a => ByteString -> Either String a
            -- JSONをパース。失敗したらLeft、成功したらRight。
            -- これは純粋関数！（IOの中で呼んでいるが、関数自体は純粋）
            pure $ eitherDecodeStrict contents
            -- $ は関数適用。 f $ x は f x と同じ。括弧を減らせる。
            -- pure (eitherDecodeStrict contents) と同じ
        else
            -- ファイルがない場合は空リストを返す（エラーではない）
            pure $ Right []

{- |
TodoリストをJSONファイルに保存する。

【IO ()の意味】
IO () は「副作用を起こすが、意味のある戻り値はない」操作。
() は「ユニット型」で、値が1つしかない型（voidに相当）。

【ByteString vs String】
String = [Char] = 文字のリンクリスト（メモリ効率悪い）
ByteString = バイト配列（メモリ効率良い、I/O向き）
Text = Unicode文字列（文字列処理向き）

実用的なHaskellでは、I/OにはByteString、文字列処理にはTextを使う。
Stringは簡単なスクリプト用。

【Lazy ByteString】
BL = Data.ByteString.Lazy
BS = Data.ByteString (Strict)

Lazy版は大きなファイルを効率的に扱える（必要な部分だけ読む）。
AesonのencodeはLazy ByteStringを返すので、BL.writeFileを使う。
-}
saveTodos :: FilePath -> TodoList -> IO ()
saveTodos path todos = BL.writeFile path (encode todos)
-- encode :: ToJSON a => a -> BL.ByteString
-- TodoListをJSON形式のByteStringに変換

{-
============================================================
                    練習問題
============================================================

1. 【観察】 loadTodos の型を声に出して読んでみましょう
   「FilePath を受け取って、IO の中に Either String TodoList がある値を返す」

2. 【実験】 REPLで試してみましょう
   $ cabal repl
   > import Todo.Storage
   > result <- loadTodos "test.json"  -- 存在しないファイル
   > result
   Right []

   > result <- loadTodos "todos.json"  -- 保存後に試す
   > result

3. 【発展】 バックアップ機能を追加してみましょう
   saveTodosWithBackup :: FilePath -> TodoList -> IO ()
   ヒント: 保存前に既存ファイルを .bak にリネーム

4. 【発展】 エラーメッセージを日本語にしてみましょう
   loadTodos の Left の場合に日本語メッセージを追加

5. 【質問】 以下のコードはなぜコンパイルエラーになる？
   badFunction :: FilePath -> TodoList
   badFunction path = loadTodos path  -- ???
   答え: IO (Either String TodoList) を TodoList に変換できない
         IOの中身は、do記法の中でしか取り出せない
-}
