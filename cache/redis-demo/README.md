# Redis Cache Demo

PostgreSQLからの超大量データに対する複雑なクエリを、Redisキャッシュあり/なしで比較するデモです。

## データ規模

### テーブル構成
- **products**: 500,000件（商品マスタ）
- **reviews**: 2,000,000件（商品レビュー、平均4件/商品）
- **orders**: 1,000,000件（注文履歴）
- **inventory_logs**: 1,500,000件（在庫変動履歴）

**合計: 500万件のレコード**

### クエリの複雑さ
- ✅ **3テーブルのJOIN** (products + reviews + orders)
- ✅ **相関サブクエリ** (inventory_logs、top reviewers)
- ✅ **GROUP BY集計** (COUNT, AVG, SUM)
- ✅ **HAVING句フィルタ**
- ✅ **複雑なソート** (複数カラム、集計結果)
- ✅ **JSONB処理**
- ✅ **文字列集約** (STRING_AGG)

## 起動方法

```bash
docker compose up --build
```

⚠️ **注意**: 初回起動時は500万件のデータ生成に**5-10分**かかります。

## アクセス

ブラウザで <http://localhost:3000> を開く

## エンドポイント

- `GET /` - デモUI
- `GET /api/products/no-cache` - DBから直接取得（100件、超重いクエリ）
- `GET /api/products/with-cache` - Redisキャッシュ経由（100件、超重いクエリ）
- `GET /api/benchmark` - 10回連続リクエストで比較
- `POST /api/cache/clear` - キャッシュクリア

## 期待される効果

- **DBアクセス**: 500-2000ms程度（複雑なJOIN + 集計のため）
- **Redisキャッシュ**: 1-5ms程度
- **速度向上**: **100-500倍以上**の劇的な高速化

## クエリの詳細

各リクエストで以下の処理を実行:

```sql
-- 3つのテーブルをJOIN
FROM products p
LEFT JOIN reviews r ON p.id = r.product_id
LEFT JOIN orders o ON p.id = o.product_id

-- 集計処理
COUNT(DISTINCT r.id)        -- レビュー数
AVG(r.rating)               -- 平均評価
COUNT(DISTINCT o.id)        -- 注文数
SUM(o.quantity)             -- 販売数
SUM(o.total_price)          -- 総売上

-- サブクエリで追加情報取得
(SELECT COUNT(*) FROM inventory_logs ...)
(SELECT STRING_AGG(...) FROM reviews ...)

-- 複雑なソート
ORDER BY AVG(r.rating) DESC, COUNT(o.id) DESC, ...
```

## 確認ポイント

1. 「DBから直接取得」を複数回クリック → 毎回500ms-2秒かかる
2. 「キャッシュクリア」してから「Redisキャッシュ経由」を複数回クリック
   - 1回目: DBアクセス（遅い、500ms-2秒）
   - 2回目以降: Redisから取得（速い、1-5ms）
3. 「ベンチマーク」で10回の平均を比較 → **100倍以上の速度差**を確認

## Redisログ監視

### 統計情報の確認

```bash
./scripts/redis-monitor.sh
```

表示される情報:
- 📊 Redis基本情報（バージョン、稼働時間）
- 🔗 クライアント接続数
- 💾 メモリ使用状況
- 🔑 保存されているキー一覧
- 📈 コマンド実行統計
- 🐌 スローログ（1ms以上のコマンド）
- 🎯 キャッシュヒット率

### リアルタイム監視

全てのRedisコマンドをリアルタイムで監視:

```bash
docker exec -it redis-demo-redis-1 redis-cli MONITOR
```

### 個別コマンドでの確認

```bash
# キャッシュヒット/ミス統計
docker exec redis-demo-redis-1 redis-cli INFO stats | grep keyspace

# スローログ確認
docker exec redis-demo-redis-1 redis-cli SLOWLOG GET 10

# メモリ使用量
docker exec redis-demo-redis-1 redis-cli INFO memory | grep used_memory_human

# 保存されているキー一覧
docker exec redis-demo-redis-1 redis-cli KEYS "*"
```
