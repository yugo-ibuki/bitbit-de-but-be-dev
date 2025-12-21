# Schema の定義方法

Atlas は複数の方法でスキーマを定義できます。

## 方法1: SQL で書く（現在の設定）

**schema/schema.sql** に直接SQLを書く：

```sql
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255)
) PRIMARY KEY (id);
```

**メリット**:
- 標準SQL、わかりやすい
- Spannerの機能を直接使える

**デメリット**:
- 冗長になりがち
- DB固有のSQLを覚える必要がある

---

## 方法2: Atlas HCL で書く（推奨）

**schema/schema.hcl** に Atlas の宣言的言語で書く：

```hcl
table "users" {
  schema = schema.main
  column "id" {
    type = bigint
    null = false
  }
  column "email" {
    type = varchar(255)
  }
  primary_key {
    columns = [column.id]
  }
}
```

**メリット**:
- DB非依存、移植性が高い
- 簡潔に書ける
- 型安全、補完が効く

**デメリット**:
- 新しい記法を覚える必要がある

### HCL を使う設定

**atlas.hcl を編集**:

```hcl
env "dev" {
  # SQLの代わりにHCLを使う
  src = "file://schema/schema.hcl"  # ← ここを変更

  url = "spanner://projects/${var.project_id}/instances/${var.instance}/databases/${var.database}"
  dev = "docker://spanner/latest"

  migration {
    dir = "file://migrations"
  }

  tx_mode = "none"
}
```

---

## 方法3: 既存DBから自動生成 ★

**既にDBがある場合、それから schema ファイルを生成できます！**

### SQLファイルとして出力

```bash
# 既存のDBからschema.sqlを生成
atlas schema inspect \
  --env dev \
  --url "spanner://projects/$GOOGLE_CLOUD_PROJECT/instances/$SPANNER_INSTANCE/databases/$SPANNER_DATABASE" \
  > schema/schema.sql

# または npm script で
npm run inspect > schema/schema.sql
```

### HCLファイルとして出力

```bash
# 既存のDBからschema.hclを生成
atlas schema inspect \
  --env dev \
  --url "spanner://projects/$GOOGLE_CLOUD_PROJECT/instances/$SPANNER_INSTANCE/databases/$SPANNER_DATABASE" \
  --format '{{ hcl . }}' \
  > schema/schema.hcl
```

---

## 比較表

| 方法 | 記述 | 移植性 | 学習コスト | 推奨用途 |
|------|------|--------|-----------|----------|
| **SQL** | 冗長 | 低（DB固有） | 低 | Spannerのみ使う場合 |
| **HCL** | 簡潔 | 高（DB非依存） | 中 | 複数DB対応、モダンな開発 |
| **既存DBから生成** | 不要 | - | 低 | 既存DBをコード化 |

---

## 実際の例

### 同じスキーマを3つの方法で

#### 1. SQL版（schema/schema.sql）

```sql
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255),
  name STRING(255),
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (id);

CREATE INDEX users_by_email ON users (email);
```

#### 2. HCL版（schema/schema.hcl）

```hcl
schema "main" {}

table "users" {
  schema = schema.main

  column "id" {
    type = bigint
    null = false
  }

  column "email" {
    type = varchar(255)
  }

  column "name" {
    type = varchar(255)
  }

  column "created_at" {
    type = timestamp
    null = false
  }

  primary_key {
    columns = [column.id]
  }

  index "users_by_email" {
    columns = [column.email]
  }
}
```

#### 3. 既存DBから生成

```bash
# すでにDBにテーブルがある場合
atlas schema inspect --env dev > schema/schema.sql
```

---

## 推奨ワークフロー

### パターンA: ゼロから開発（HCL推奨）

```bash
# 1. HCLでスキーマを書く
vim schema/schema.hcl

# 2. atlas.hcl を編集して HCL を使うように設定
vim atlas.hcl  # src = "file://schema/schema.hcl"

# 3. 適用
npm run diff
npm run apply
```

### パターンB: 既存DBをコード化（inspect推奨）

```bash
# 1. 既存DBからHCLを生成
atlas schema inspect --env dev --format '{{ hcl . }}' > schema/schema.hcl

# 2. atlas.hcl を編集
vim atlas.hcl  # src = "file://schema/schema.hcl"

# 3. 以降はHCLを編集して管理
vim schema/schema.hcl
npm run apply
```

### パターンC: Spannerに特化（SQL推奨）

```bash
# 現在の設定のまま
vim schema/schema.sql
npm run apply
```

---

## HCL を使う利点

### 1. DB非依存

同じHCLで複数のDBに対応：

```hcl
table "users" {
  column "id" {
    type = bigint  # ← これがSpannerではINT64、PostgreSQLではBIGINTに
  }
}
```

### 2. 変数と関数が使える

```hcl
variable "timestamp_precision" {
  type    = number
  default = 6
}

table "users" {
  column "created_at" {
    type = timestamp(var.timestamp_precision)
  }
}
```

### 3. 再利用可能

```hcl
# common.hcl
locals {
  timestamp_columns = {
    created_at = {
      type = timestamp
      null = false
    }
    updated_at = {
      type = timestamp
      null = false
    }
  }
}

# schema.hcl
table "users" {
  column "id" { type = bigint }
  dynamic "column" {
    for_each = local.timestamp_columns
    content {
      name = column.key
      type = column.value.type
      null = column.value.null
    }
  }
}
```

---

## まとめ

| やりたいこと | 使う方法 |
|------------|---------|
| ゼロから開発（モダン） | **HCL** で書く |
| ゼロから開発（シンプル） | **SQL** で書く |
| 既存DBをコード化 | **inspect** で生成 |
| 複数DB対応 | **HCL** で書く |
| Spannerのみ使う | **SQL** でOK |

**次のステップ**: HCLを試したい場合は、例を追加しましょうか？
