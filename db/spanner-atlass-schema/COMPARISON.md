# SQL vs HCL 比較環境

両方のスキーマ定義方法を同じDBで試せます。

## 🎯 クイックスタート

### SQL版を試す

```bash
# 1. スキーマを編集
vim schema/schema.sql

# 2. 差分確認
npm run sql:diff

# 3. 適用
npm run sql:apply
```

### HCL版を試す

```bash
# 1. スキーマを編集
vim schema/schema.hcl

# 2. 差分確認
npm run hcl:diff

# 3. 適用
npm run hcl:apply
```

---

## 📋 コマンド対応表

| 操作 | SQL版 | HCL版 |
|------|-------|-------|
| **差分確認** | `npm run sql:diff` | `npm run hcl:diff` |
| **適用（自動承認）** | `npm run sql:apply` | `npm run hcl:apply` |
| **適用（確認あり）** | `npm run sql:apply:check` | `npm run hcl:apply:check` |
| **検証** | `npm run sql:validate` | `npm run hcl:validate` |
| **リント** | `npm run sql:lint` | `npm run hcl:validate` |

**エイリアス（デフォルトはSQL版）**:
- `npm run diff` → `npm run sql:diff`
- `npm run apply` → `npm run sql:apply`

---

## 🔄 実際の比較ワークフロー

### ステップ1: 既存DBからHCLを生成

```bash
# 現在のDBからHCLを生成
npm run inspect:hcl

# 生成されたファイルを確認
cat schema/schema-generated.hcl

# メインのHCLファイルにコピー（初回のみ）
cp schema/schema-generated.hcl schema/schema.hcl
```

### ステップ2: 同じ変更を両方で試す

#### SQL版で試す

```bash
# schema.sql を編集（例：bio カラムを追加）
vim schema/schema.sql

# 差分確認
npm run sql:diff

# 適用
npm run sql:apply
```

#### HCL版で同じ変更を試す

```bash
# schema.hcl を編集（例：bio カラムを追加）
vim schema/schema.hcl

# 差分確認（既にSQL版で適用済みなら差分なし）
npm run hcl:diff

# 適用
npm run hcl:apply
```

---

## 📝 同じスキーマを両方で書く例

### テーブルにカラムを追加

#### SQL版（schema/schema.sql）

```sql
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255),
  display_name STRING(255),
  bio STRING(1024),  -- ← 追加
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  updated_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (id);
```

#### HCL版（schema/schema.hcl）

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

  column "display_name" {
    type = varchar(255)
  }

  column "bio" {              // ← 追加
    type = varchar(1024)
  }

  column "created_at" {
    type = timestamp
    null = false
  }

  column "updated_at" {
    type = timestamp
    null = false
  }

  primary_key {
    columns = [column.id]
  }
}
```

---

## 🧪 検証方法

### 1. SQL版とHCL版が同じDBを生成するか確認

```bash
# SQL版を適用
npm run sql:apply

# DBの状態を確認
npm run inspect

# HCL版で差分確認（差分がなければ同じ）
npm run hcl:diff
```

### 2. どちらを使っているか確認

コマンド実行時に絵文字で表示されます：

```bash
$ npm run sql:diff
📝 Using SQL schema...

$ npm run hcl:diff
🔷 Using HCL schema...
```

---

## 📂 ファイル構成

```
schema/
  ├── schema.sql               # SQL版（手動編集）
  ├── schema.hcl               # HCL版（手動編集）
  ├── schema-generated.sql     # DB→SQL生成（参照用）
  └── schema-generated.hcl     # DB→HCL生成（参照用）

atlas.hcl          # SQL版用の設定（env "dev"）
atlas-hcl.hcl      # HCL版用の設定（env "dev-hcl"）

migrations/        # SQL版のマイグレーション
migrations-hcl/    # HCL版のマイグレーション（使う場合）
```

---

## 💡 使い分けのコツ

### SQL版が向いている場合
- Spanner固有の機能を使いたい（INTERLEAVE、change streams など）
- SQLに慣れている
- チームがSQLを好む

### HCL版が向いている場合
- 複数DBに移植する可能性がある
- 型安全性が欲しい
- コードレビューしやすくしたい
- 変数や関数を使いたい

---

## 🚀 実践的な使い方

### パターン1: 開発中に両方試す

```bash
# SQL版で素早く試す
vim schema/schema.sql
npm run sql:diff
npm run sql:apply

# 同じ変更をHCL版でも書いてみる
vim schema/schema.hcl
npm run hcl:diff  # 差分なしを確認

# どちらが書きやすいか比較
```

### パターン2: DBから生成→どちらかを選ぶ

```bash
# 既存DBから両方生成
npm run inspect:sql
npm run inspect:hcl

# ファイルを比較
diff schema/schema-generated.sql schema/schema.sql
cat schema/schema-generated.hcl

# 好きな方をメインにする
# SQL派: cp schema/schema-generated.sql schema/schema.sql
# HCL派: cp schema/schema-generated.hcl schema/schema.hcl
```

### パターン3: 片方をマスターにする

```bash
# HCLをマスターとして管理
vim schema/schema.hcl
npm run hcl:apply

# SQL版は参照用として生成
npm run inspect:sql
# → schema/schema-generated.sql を git commit
```

---

## ⚠️ 注意事項

1. **同じDBに適用する**: 両方とも同じ Spanner DB を参照します
2. **マイグレーションディレクトリが別**: SQL版とHCL版で別のディレクトリを使用
3. **どちらか一方を選ぶのが推奨**: 両方同時に管理すると複雑になります

---

## 🎓 学習の流れ

1. **まずSQL版で慣れる**
   ```bash
   npm run sql:diff
   npm run sql:apply
   ```

2. **DBからHCLを生成して比較**
   ```bash
   npm run inspect:hcl
   cat schema/schema-generated.hcl
   ```

3. **HCL版を試す**
   ```bash
   vim schema/schema.hcl
   npm run hcl:diff
   npm run hcl:apply
   ```

4. **好きな方を選ぶ**
   - SQL派: `npm run sql:*` を継続使用
   - HCL派: `npm run hcl:*` に移行

---

## 📚 参考

- [SCHEMA_SOURCES.md](./SCHEMA_SOURCES.md) - 詳細な比較
- [WORKFLOW.md](./WORKFLOW.md) - 基本ワークフロー
- [Atlas HCL Schema](https://atlasgo.io/atlas-schema/hcl) - HCL公式ドキュメント
