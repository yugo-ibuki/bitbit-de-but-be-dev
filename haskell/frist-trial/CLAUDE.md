# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

これは **Haskell学習プロジェクト** です。Todo CLIアプリケーションを通じて、関数型プログラミングの概念を実践的に学びます。

## ビルドコマンド

```bash
# 依存関係のインストールとビルド
cabal build

# アプリケーションの実行
cabal run todo-cli -- <コマンド>
# 例:
cabal run todo-cli -- add        # タスク追加（対話式）
cabal run todo-cli -- list       # タスク一覧
cabal run todo-cli -- complete 1 # ID:1を完了にする
cabal run todo-cli -- delete 1   # ID:1を削除

# テスト実行
cabal test

# 特定のテストのみ実行
cabal test --test-option=--match="/nextId/"

# REPLで対話的に試す
cabal repl

# ビルド成果物の削除
cabal clean
```

## 前提条件

GHCup（Haskellツールチェーン）のインストール:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## アーキテクチャ

```
src/
├── Main.hs           -- エントリーポイント、CLI解析、IO操作
└── Todo/
    ├── Types.hs      -- データ型定義（TodoItem, Priority, Status）
    ├── Commands.hs   -- 純粋なビジネスロジック（追加、削除、フィルタ）
    └── Storage.hs    -- JSONファイル永続化
test/
├── Spec.hs           -- テスト検出エントリーポイント
└── Todo/
    └── TypesSpec.hs  -- Commandsモジュールのユニットテスト
```

### 学習する概念（ファイル別）

| モジュール | 学べるHaskellの概念 |
|-----------|-------------------|
| `Types.hs` | 代数的データ型(ADT)、レコード、deriving、Generic |
| `Commands.hs` | 高階関数、パターンマッチ、Maybe、ラムダ式 |
| `Storage.hs` | IOモナド、Either、do記法、ByteString |
| `Main.hs` | case式、ユーザー入出力、文字列操作 |

### 設計の重要ポイント

1. **純粋/非純粋の分離**: `Commands.hs`は純粋関数のみ（IOなし）。副作用は`Main.hs`と`Storage.hs`に隔離。

2. **Eitherによるエラー処理**: `loadTodos`は例外を投げず`Either String TodoList`を返す → エラー処理を強制。

3. **Maybeによるオプション値**: `todoDesc`は`Maybe Text`を使用 → null値が存在しない安全な設計。

## 学習順序（推奨）

ソースファイルには詳細な日本語コメントが付いています。以下の順序で読むことを推奨:

1. **Types.hs** - ADT、レコード、型クラス
2. **Commands.hs** - 高階関数、パターンマッチ
3. **Storage.hs** - IOモナド、Either
4. **Main.hs** - すべてを組み合わせる

## 拡張アイデア（学習用）

難易度順:

1. 期限日の追加（`Data.Time`の練習）
2. タグ機能の追加（`[Text]`リストの操作）
3. 優先度/日付でのソート（`sortBy`の使用）
4. `edit`コマンドの追加（レコード更新構文）
5. 繰り返しタスク（Stateモナド入門）
6. `undo`コマンド（Zipperパターン入門）
