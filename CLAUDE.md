# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

**技術学習・PoC用のリポジトリ**。各ディレクトリが完全に独立したプロジェクトであり、相互依存や共通設定は存在しない。

## コマンドパターン

各サブプロジェクトは独立しているため、そのディレクトリ内で実行:

### TypeScript プロジェクト (npm)
```bash
npm install
npm run dev      # 開発実行
npm run build    # ビルド
npm run typecheck  # 型チェック（あれば）
```

### Go プロジェクト
```bash
go mod tidy
go run .
go test ./...
```

### Haskell プロジェクト
```bash
cabal build
cabal run todo-cli -- <コマンド>
cabal test
cabal repl  # REPL
```

### Docker Compose があるプロジェクト
```bash
docker-compose up -d
docker-compose down
```

## 注意事項

- 各ディレクトリは完全に独立（依存関係共有なし）
- 一部のプロジェクトには個別の CLAUDE.md あり（`haskell/frist-trial/`, `ts/functional-domain-modeling/`）
- 環境変数が必要な場合は `.env.example` を `.env` にコピー
