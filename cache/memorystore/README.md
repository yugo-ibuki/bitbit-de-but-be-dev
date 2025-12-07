# Cloud Run + Memorystore Redis デモ

キャッシュの効果を体験できる最小構成。

## 構成

- **Cloud Run**: Fastify API サーバー
- **Memorystore for Redis**: キャッシュ（BASIC tier, 1GB）
- **VPC Connector**: Cloud Run から Redis への接続

## デプロイ手順

### 1. 前提条件

```fish
# gcloudコマンドで対象のプロジェクトを選択しておく
gcloud config set project <projectId>

# 必要なAPIを有効化
gcloud services enable \
  run.googleapis.com \
  redis.googleapis.com \
  vpcaccess.googleapis.com \
  artifactregistry.googleapis.com \
  compute.googleapis.com

# Terraform用の認証設定（これをしないとプロジェクトIDが自動取得できません）
gcloud auth application-default login
```

### 2. コンテナイメージをビルド・プッシュ

```fish
# Artifact Registry リポジトリ作成（初回のみ）
gcloud artifacts repositories create redis-demo \
  --repository-format=docker \
  --location=asia-northeast1

# ビルド＆プッシュ
gcloud builds submit \
  --tag asia-northeast1-docker.pkg.dev/(gcloud config get-value project)/redis-demo/app:latest
```

### 3. Terraform でインフラ構築

`.env` ファイルを作成し、プロジェクトIDを設定します。

```fish
cp .env.example .env
# .env を編集して TF_VAR_project_id を設定してください
```

Terraform コマンドを実行（Bun経由で実行することで .env が読み込まれます）：

```fish
bun run tf:init
bun run tf:plan
bun run tf:apply
```

## 動作確認

```fish
set URL (terraform output -raw service_url)

# 1回目: キャッシュミス（約2秒かかる）
curl $URL/data/test1
# {"source":"computed","elapsed_ms":2003,"data":{...}}

# 2回目: キャッシュヒット（数ms）
curl $URL/data/test1
# {"source":"cache","elapsed_ms":2,"data":{...}}

# キャッシュ削除
curl -X DELETE $URL/data/test1

# ヘルスチェック
curl $URL/health
```

## クリーンアップ

```fish
terraform destroy
```

## 注意事項

- Memorystore Redis の作成には5-10分かかる
- BASIC tier は SLA なし（本番は STANDARD_HA を推奨）
- VPC Connector にも課金あり（最小構成で約$7/月）
