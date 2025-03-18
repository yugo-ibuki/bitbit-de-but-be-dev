# Zen テストプロジェクト

このプロジェクトは、[Hypersequent/zen](https://github.com/Hypersequent/zen) ライブラリの基本的な機能をテストするためのものです。Zenは、Goで非同期および並行ワークフローを扱うためのライブラリです。

## セットアップ

```bash
# リポジトリをクローン
git clone <your-repo-url>
cd zen-test-project

# 依存関係をインストール
go mod tidy
```

## 実行方法

このプロジェクトには3つのサンプルが含まれています：

```bash
# 基本的な使用例
go run cmd/basic/main.go

# エラーハンドリングの例
go run cmd/error-handling/main.go

# 並行処理の例
go run cmd/concurrent/main.go
```

## サンプルの内容

### 基本的な使用例 (cmd/basic/main.go)

- 単純なタスクの定義と実行
- タスクの連鎖

### エラーハンドリング (cmd/error-handling/main.go)

- リトライ機能
- タイムアウト機能
- エラーハンドリングのパターン

### 並行処理 (cmd/concurrent/main.go)

- 並列処理 (Parallelize)
- シーケンシャル処理
- 複雑なワークフローの構築

## Zenライブラリの主な機能

- `NewTask`: 非同期タスクを定義
- `WithRetry`: 失敗時に自動的に再試行
- `WithTimeout`: タスクにタイムアウトを設定
- `Parallelize`: 複数のタスクを並列実行

詳細については、[Zen公式リポジトリ](https://github.com/Hypersequent/zen)を参照してください。
