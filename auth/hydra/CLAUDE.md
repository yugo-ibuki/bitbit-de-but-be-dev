# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

Ory Hydra を使った OAuth2/OIDC 認証基盤の PoC 環境。Hydra 本体と、カスタムのログイン/同意 UI (Express + TypeScript) で構成される。

## コマンド

### 環境起動
```bash
cp .env.example .env  # 初回のみ
docker-compose up -d
```

### OAuth2 クライアント登録
```bash
./scripts/init-clients.sh
```

### Login/Consent UI のローカル開発
```bash
cd login-consent-ui
npm install
npm run dev  # ts-node で直接実行
```

### ビルド
```bash
cd login-consent-ui
npm run build
npm start
```

## アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                      OAuth2 Flow                             │
├─────────────────────────────────────────────────────────────┤
│  Client App                                                  │
│      │                                                       │
│      ▼                                                       │
│  ┌──────────────┐    ┌───────────────────┐                  │
│  │ Hydra        │◄───┤ PostgreSQL        │                  │
│  │ :4444 Public │    │ (セッション/クラ  │                  │
│  │ :4445 Admin  │    │  イアント保存)    │                  │
│  └──────┬───────┘    └───────────────────┘                  │
│         │                                                    │
│         │ login_challenge / consent_challenge               │
│         ▼                                                    │
│  ┌──────────────┐                                           │
│  │ Login/Consent│    ← カスタム UI (Express)                │
│  │ UI :3000     │    ← Hydra Admin API を呼び出し           │
│  └──────────────┘                                           │
└─────────────────────────────────────────────────────────────┘
```

### コンポーネント

| サービス | ポート | 役割 |
|---------|--------|-----|
| hydra | 4444 | OAuth2 Public API (認可エンドポイント等) |
| hydra | 4445 | Admin API (クライアント管理、ログイン/同意の accept/reject) |
| login-consent-ui | 3030 | ログイン/同意画面 (Hydra からリダイレクトされる) |
| postgres | 5432 | Hydra のデータ永続化 |

### Login/Consent UI の処理フロー

1. Hydra が `login_challenge` 付きで `/login` にリダイレクト
2. UI が Hydra Admin API でチャレンジ情報を取得
3. ユーザー認証後、`/admin/oauth2/auth/requests/login/accept` を呼び出し
4. Hydra が `consent_challenge` 付きで `/consent` にリダイレクト
5. ユーザー同意後、`/admin/oauth2/auth/requests/consent/accept` を呼び出し
6. Hydra が認可コードを発行しクライアントへリダイレクト

## 認証テスト

デモユーザー:
- Email: `user@example.com`
- Password: `password`

認可フロー開始 URL:
```
http://localhost:4444/oauth2/auth?client_id=demo-client&response_type=code&scope=openid%20profile%20email&redirect_uri=http://127.0.0.1:9999/callback&state=random-state
```

## 環境変数

| 変数 | 説明 |
|-----|-----|
| `HYDRA_ADMIN_URL` | Hydra Admin API の URL (UI から参照) |
| `SECRETS_SYSTEM` | Hydra の暗号化シークレット (32文字以上) |
| `URLS_LOGIN` / `URLS_CONSENT` / `URLS_LOGOUT` | UI のエンドポイント URL |

## DBアクセス

### 接続コマンド

```bash
# 直接クエリ実行
docker exec hydra-postgres psql -U hydra -d hydra -c "SELECT * FROM テーブル名"

# 対話モードで接続
docker exec -it hydra-postgres psql -U hydra -d hydra
```

### 対話モード内のコマンド

| コマンド | 説明 |
|---------|------|
| `\dt` | テーブル一覧 |
| `\d テーブル名` | テーブル構造 |
| `\d+ テーブル名` | 詳細表示（インデックス・外部キー含む） |
| `\di` | インデックス一覧 |
| `\q` | 終了 |

### 主要テーブル

| テーブル | 内容 |
|---------|------|
| `hydra_client` | OAuthクライアント |
| `hydra_oauth2_authentication_session` | ログインセッション |
| `hydra_oauth2_access` | アクセストークン |
| `hydra_oauth2_refresh` | リフレッシュトークン |
| `hydra_oauth2_code` | 認可コード |
| `hydra_oauth2_flow` | 認可フロー全体の状態 |
| `hydra_jwk` | JWT署名用の鍵 |

### よく使うクエリ

```bash
# クライアント一覧
docker exec hydra-postgres psql -U hydra -d hydra -c "SELECT id, client_name, scope FROM hydra_client"

# ログインセッション
docker exec hydra-postgres psql -U hydra -d hydra -c "SELECT * FROM hydra_oauth2_authentication_session"

# アクセストークン
docker exec hydra-postgres psql -U hydra -d hydra -c "SELECT client_id, subject, scope, active FROM hydra_oauth2_access"

# 認可フロー
docker exec hydra-postgres psql -U hydra -d hydra -c "SELECT * FROM hydra_oauth2_flow"
```
