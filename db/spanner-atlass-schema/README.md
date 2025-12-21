# Atlas Spanner Schema Management PoC

このプロジェクトは、[Atlas](https://atlasgo.io/) を使用して Google Cloud Spanner のスキーマを管理する Proof of Concept (PoC) です。

## 📋 前提条件

- **Docker**: Atlas の dev-url として使用
- **Google Cloud SDK**: Spanner インスタンスとデータベースの管理
- **Atlas CLI (beta)**: Spanner サポートはベータ版で提供
- **Atlas Pro アカウント** (推奨): 高度な機能を使用する場合

## 🚀 セットアップ

### 1. Atlas CLI のインストール

```bash
# ベータ版の Atlas をインストール (Spanner サポート含む)
curl -sSf https://atlasgo.sh | ATLAS_VERSION="beta" sh
```

### 2. Google Cloud SDK の設定

```bash
# Google Cloud SDK のインストール (未インストールの場合)
# https://cloud.google.com/sdk/docs/install

# 認証
gcloud auth login

# プロジェクトの設定
gcloud config set project YOUR_PROJECT_ID
```

### 3. 環境変数の設定

```bash
# .env.example をコピー
cp .env.example .env

# .env ファイルを編集して、あなたの GCP プロジェクト情報を設定
# GOOGLE_CLOUD_PROJECT=your-project-id
# SPANNER_INSTANCE=your-instance-name
# SPANNER_DATABASE=your-database-name

# 環境変数を読み込む
source .env
```

### 4. npm パッケージのインストール (オプション)

```bash
npm install
```

### 5. Spanner インスタンスとデータベースの作成

自動セットアップスクリプトを使用:

```bash
npm run setup
# または
./scripts/setup.sh
```

または手動で作成:

```bash
# Spanner インスタンスの作成
gcloud spanner instances create your-instance-name \
  --config=regional-us-central1 \
  --description="Atlas PoC Instance" \
  --nodes=1

# データベースの作成
gcloud spanner databases create your-database-name \
  --instance=your-instance-name
```

## 📁 プロジェクト構造

```
.
├── README.md                    # このファイル
├── package.json                 # npm スクリプト定義
├── atlas.hcl                    # Atlas 設定ファイル
├── .env.example                 # 環境変数のサンプル
├── schema/                      # スキーマ定義
│   ├── schema.sql              # メインスキーマ
│   └── examples/               # サンプルスキーマ
│       ├── ecommerce.sql       # E-commerce スキーマ例
│       └── blog-with-comments.sql  # ブログスキーマ例
├── migrations/                  # 生成されたマイグレーション
└── scripts/                     # ヘルパースクリプト
    ├── setup.sh                # 初期セットアップ (GCP リソース作成)
    └── help.sh                 # ヘルプ表示
```

**注**: ほとんどの操作は Atlas CLI を直接使用します。npm scripts が便利なエイリアスを提供します。

## 💡 使い方

> **📖 詳細なワークフロー**: [WORKFLOW.md](./WORKFLOW.md) をご覧ください
> **📖 スキーマ定義方法**: [SCHEMA_SOURCES.md](./SCHEMA_SOURCES.md) をご覧ください

### スキーマ定義の選択肢

Atlas は3つの方法でスキーマを定義できます：

1. **📝 SQL で書く**: `schema/schema.sql` → `npm run sql:*`
2. **🔷 HCL で書く**: `schema/schema.hcl` → `npm run hcl:*`
3. **🔄 既存DBから生成**: `npm run inspect:sql` または `npm run inspect:hcl`

**両方試せる環境になっています！** 詳細は [COMPARISON.md](./COMPARISON.md) 参照

### 基本的な流れ

1. **スキーマファイルを編集**（あなたが書く、または既存DBから生成）
2. **Atlas で差分確認**（Atlas が自動計算）
3. **適用**（Atlas が自動実行）

### クイックスタート (npm コマンド)

すべてのコマンドを確認:

```bash
npm run help
```

よく使うコマンド:

```bash
# 初期セットアップ
source .env
npm run setup

# SQL版を試す
npm run sql:diff
npm run sql:apply

# HCL版を試す
npm run hcl:diff
npm run hcl:apply

# DBからスキーマを生成
npm run inspect:sql  # → schema/schema-generated.sql
npm run inspect:hcl  # → schema/schema-generated.hcl

# マイグレーション
npm run migrate:new add_new_table
npm run migrate:status
npm run migrate:apply
npm run migrate:down
```

### 利用可能な npm スクリプト

#### Setup & Inspection

| コマンド | 説明 |
|---------|------|
| `npm run setup` | 初期セットアップ (インスタンス・データベース作成) |
| `npm run inspect` | 現在のデータベーススキーマを検査 |
| `npm run inspect:sql` | DBからSQLファイルを生成 → `schema/schema-generated.sql` |
| `npm run inspect:hcl` | DBからHCLファイルを生成 → `schema/schema-generated.hcl` |

#### Schema Management (SQL)

| コマンド | 説明 |
|---------|------|
| `npm run diff` | データベースとスキーマファイルの差分を表示 |
| `npm run apply` | スキーマ変更を適用 (自動承認) |
| `npm run apply:check` | スキーマ変更を適用 (確認あり) |
| `npm run apply:prod` | スキーマ変更を適用 (prod環境) |
| `npm run validate` | スキーマファイルを検証 |
| `npm run lint` | スキーマをベストプラクティスでチェック |

#### Schema Management (HCL)

| コマンド | 説明 |
|---------|------|
| `npm run hcl:diff` | HCLスキーマの差分を表示 |
| `npm run hcl:apply` | HCLスキーマを適用 |

#### Migration Management

| コマンド | 説明 |
|---------|------|
| `npm run migrate:new` | 新しいマイグレーションを生成 |
| `npm run migrate:status` | マイグレーション状態を確認 |
| `npm run migrate:apply` | 保留中のマイグレーションを適用 (自動承認) |
| `npm run migrate:apply:check` | 保留中のマイグレーションを適用 (確認あり) |
| `npm run migrate:down` | 最後のマイグレーションをロールバック |

#### Utilities

| コマンド | 説明 |
|---------|------|
| `npm run clean` | 生成されたマイグレーションファイルを削除 |
| `npm run help` | 利用可能なコマンド一覧を表示 |

### Atlas CLI を直接使う場合

#### スキーマの検査

現在のデータベーススキーマを確認:

```bash
atlas schema inspect \
  --env dev \
  --url "spanner://projects/${GOOGLE_CLOUD_PROJECT}/instances/${SPANNER_INSTANCE}/databases/${SPANNER_DATABASE}"
```

### スキーマの差分確認

現在のデータベースと定義ファイルの差分を確認:

```bash
atlas schema diff \
  --env dev \
  --from "spanner://projects/${GOOGLE_CLOUD_PROJECT}/instances/${SPANNER_INSTANCE}/databases/${SPANNER_DATABASE}" \
  --to file://schema/schema.sql \
  --dev-url "docker://spanner/latest"
```

### スキーマの適用

定義ファイルの内容をデータベースに適用:

```bash
# ヘルパースクリプトを使用 (推奨)
./scripts/apply.sh

# または直接実行
atlas schema apply \
  --env dev \
  --to file://schema/schema.sql \
  --dev-url "docker://spanner/latest" \
  --tx-mode=none
```

**重要**: Spanner は DDL ステートメントでトランザクションをサポートしていないため、`--tx-mode=none` フラグが必要です。

### マイグレーションの生成

バージョン管理されたマイグレーションを生成:

```bash
atlas migrate diff migration_name \
  --env dev \
  --to file://schema/schema.sql \
  --dev-url "docker://spanner/latest"
```

### マイグレーションの適用

生成されたマイグレーションをデータベースに適用:

```bash
atlas migrate apply \
  --env dev \
  --url "spanner://projects/${GOOGLE_CLOUD_PROJECT}/instances/${SPANNER_INSTANCE}/databases/${SPANNER_DATABASE}" \
  --tx-mode=none
```

## 📝 スキーマ例

### メインスキーマ (`schema/schema.sql`)

基本的なブログシステムのスキーマ:
- `users` テーブル: ユーザー情報
- `posts` テーブル: ブログ投稿
- インデックス: 著者別、作成日時別

### E-commerce スキーマ (`schema/examples/ecommerce.sql`)

E-commerce システムのスキーマ例:
- Interleaved Tables の使用例
- `customers`, `orders`, `order_items`, `products` テーブル
- 親子関係とカスケード削除の設定

### ブログとコメント (`schema/examples/blog-with-comments.sql`)

3層の Interleaved Tables の例:
- `authors` → `blog_posts` → `comments` の階層構造
- タグ機能付き

## 🔧 Spanner 固有の機能

### Interleaved Tables

親テーブルと物理的に近くに配置することでパフォーマンスを向上:

```sql
CREATE TABLE order_items (
  customer_id INT64 NOT NULL,
  order_id INT64 NOT NULL,
  item_id INT64 NOT NULL,
  ...
) PRIMARY KEY (customer_id, order_id, item_id),
  INTERLEAVE IN PARENT orders ON DELETE CASCADE;
```

### Commit Timestamp

自動的にコミット時刻を記録:

```sql
created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
```

### Global Indexes

複数リージョンにまたがるインデックス:

```sql
CREATE INDEX global_idx ON table_name (column_name);
```

## 🧪 サンプルスキーマの試用

異なるスキーマ例を試すには、`schema/schema.sql` を編集するか、examples からコピー:

```bash
# E-commerce スキーマを試す
cp schema/examples/ecommerce.sql schema/schema.sql
npm run apply

# ブログスキーマを試す
cp schema/examples/blog-with-comments.sql schema/schema.sql
npm run apply
```

## 🔄 ワークフロー例

### 宣言的アプローチ (Declarative)

スキーマファイルを編集して直接適用:

```bash
# 1. スキーマファイルを編集
vim schema/schema.sql

# 2. 変更内容を確認
npm run diff

# 3. 適用
npm run apply
```

### マイグレーションアプローチ (Versioned)

バージョン管理されたマイグレーションを生成:

```bash
# 1. スキーマファイルを編集
vim schema/schema.sql

# 2. マイグレーションファイルを生成
npm run migrate:new add_email_column

# 3. マイグレーション状態を確認
npm run migrate:status

# 4. マイグレーションを適用
npm run migrate:apply
```

### ロールバック

```bash
# 最後のマイグレーションをロールバック
npm run migrate:down

# 特定のバージョンまでロールバック (Atlas CLI を直接使用)
atlas migrate down --env dev --to-version 20240101000000
```

## 📚 参考資料

- [Atlas Documentation](https://atlasgo.io/docs)
- [Atlas Spanner Guide](https://atlasgo.io/guides/spanner/automatic-migrations)
- [Google Cloud Spanner Documentation](https://cloud.google.com/spanner/docs)
- [Atlas v0.36 Release Notes](https://atlasgo.io/blog/2025/07/21/v036-snowflake-postgres-partitions-and-azure-devops) (Spanner サポート追加)
- [Atlas v0.38 Release Notes](https://atlasgo.io/blog/2025/10/28/v038-analyzers-pii-and-migration-hooks) (Spanner 機能拡張)

## ⚠️ 注意事項

1. **ベータ機能**: Spanner サポートは現在ベータ版です
2. **トランザクション無効**: `--tx-mode=none` フラグが必須
3. **コスト**: Spanner インスタンスは課金されます。不要な場合は削除してください
4. **リージョン**: デフォルトは `us-central1` です。必要に応じて変更してください

## 🧹 クリーンアップ

リソースの削除:

```bash
# データベースの削除
gcloud spanner databases delete ${SPANNER_DATABASE} --instance=${SPANNER_INSTANCE}

# インスタンスの削除 (課金を停止)
gcloud spanner instances delete ${SPANNER_INSTANCE}
```

## 📄 ライセンス

このプロジェクトは PoC 目的で作成されています。

## Sources

- [Automatic Google Cloud Spanner Schema Migrations with Atlas](https://atlasgo.io/guides/spanner/automatic-migrations)
- [Atlas v0.36: Snowflake Beta, PostgreSQL Partitions, Azure DevOps, and More](https://atlasgo.io/blog/2025/07/21/v036-snowflake-postgres-partitions-and-azure-devops)
- [Atlas v0.38: Linting Analyzers, PII Detection, Migration Hooks, and More](https://atlasgo.io/blog/2025/10/28/v038-analyzers-pii-and-migration-hooks)
