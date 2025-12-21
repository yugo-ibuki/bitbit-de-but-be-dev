# Atlas Spanner ワークフロー

## 基本的な流れ

### 📝 1. SQLファイルを書く（手動）

`schema/schema.sql` に理想の状態を書きます：

```sql
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255),
  name STRING(255)
) PRIMARY KEY (id);
```

### 🤖 2. Atlas を使う（自動）

Atlas があなたの SQL を見て、現在の DB との差分を自動計算します。

## 2つのアプローチ

### アプローチ A: 宣言的（シンプル）

**あなた**: 「こういうスキーマにしたい」（SQLを書く）
**Atlas**: 「OK、現在のDBと比較して必要な変更を適用します」

```bash
# 1. schema.sql を編集（手動）
vim schema/schema.sql

# 2. 差分確認（Atlas が自動計算）
npm run diff

# 3. 適用（Atlas が自動で DDL 実行）
npm run apply
```

**メリット**: シンプル、すぐ適用できる
**デメリット**: マイグレーション履歴が残らない

---

### アプローチ B: マイグレーション管理（本番向け）

**あなた**: 「こういうスキーマにしたい」（SQLを書く）
**Atlas**: 「マイグレーションファイル（SQL）を生成します」
**あなた**: 「マイグレーションファイルを確認して適用」

```bash
# 1. schema.sql を編集（手動）
vim schema/schema.sql

# 2. マイグレーションファイル生成（Atlas が自動生成）
npm run migrate:new add_users_table
# → migrations/20240101_add_users_table.sql が作成される

# 3. 生成されたマイグレーションを確認（手動）
cat migrations/20240101_add_users_table.sql

# 4. マイグレーション適用（Atlas が実行）
npm run migrate:apply
```

**メリット**: 履歴が残る、ロールバック可能、チーム開発に最適
**デメリット**: 少し手間が増える

## 具体例

### 例：新しいカラムを追加

#### 手順 1: SQL を編集（手動）

```bash
vim schema/schema.sql
```

```sql
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255),
  name STRING(255),
  created_at TIMESTAMP NOT NULL  -- ← 追加
) PRIMARY KEY (id);
```

#### 手順 2: Atlas で確認

```bash
npm run diff
```

**Atlas の出力例**:
```diff
-- Planned Changes:
+ ALTER TABLE users ADD COLUMN created_at TIMESTAMP NOT NULL
```

#### 手順 3: 適用方法を選ぶ

**パターン A: すぐ適用**
```bash
npm run apply
# → DBに直接適用される
```

**パターン B: マイグレーション経由**
```bash
npm run migrate:new add_created_at
# → migrations/20240101_add_created_at.sql が生成される

cat migrations/20240101_add_created_at.sql
# 中身を確認

npm run migrate:apply
# → マイグレーションが適用される
```

## よくある質問

### Q1: schema.sql は手動で書くの？
**A**: はい。あなたが理想の状態を SQL で書きます。

### Q2: Atlas は何をするの？
**A**:
- 現在の DB を調べる
- あなたの schema.sql と比較
- 必要な DDL（ALTER TABLE など）を自動計算
- 適用する

### Q3: マイグレーションファイルは手動で書くの？
**A**: いいえ。Atlas が自動生成します。あなたは schema.sql だけ書けば OK。

### Q4: どっちのアプローチを使えばいい？
**A**:
- **開発・検証**: 宣言的（すぐ試せる）
- **本番・チーム開発**: マイグレーション管理（履歴が残る）

## フローチャート

```
┌─────────────────────────┐
│ schema.sql を編集       │ ← あなたの作業
│ (理想の状態を書く)      │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ npm run diff            │ ← Atlas が差分計算
│ (何が変わるか確認)      │
└───────────┬─────────────┘
            │
            ├─────────────┐
            ▼             ▼
    ┌───────────┐   ┌──────────────┐
    │ 宣言的    │   │ マイグレーション │
    └─────┬─────┘   └───────┬──────┘
          │                 │
          ▼                 ▼
    npm run apply    npm run migrate:new
    (直接適用)       (ファイル生成)
                           │
                           ▼
                    npm run migrate:apply
                    (ファイルから適用)
```

## 推奨ワークフロー

### 開発環境
```bash
# 1. SQLを書く
vim schema/schema.sql

# 2. すぐ試す
npm run diff
npm run apply
```

### 本番環境
```bash
# 1. SQLを書く
vim schema/schema.sql

# 2. マイグレーション生成
npm run migrate:new describe_change

# 3. レビュー
cat migrations/*.sql
git add migrations/
git commit -m "Add migration: describe_change"

# 4. 本番適用
npm run migrate:apply
```

## まとめ

**Atlas の役割**: あなたが書いた SQL（理想の状態）と現在の DB を比較して、必要な変更を自動計算・適用する

**あなたの役割**: `schema/schema.sql` に理想の状態を書くだけ ✍️
