# ElasticSearch学習プロジェクト

TypeScriptでElasticSearchの基本機能を学習するための実践的なプロジェクトです。

## 🎯 学習目標

このプロジェクトを通じて以下の技術を習得できます：

- ElasticSearchの基本概念（インデックス、ドキュメント、マッピング）
- TypeScriptでのElasticSearchクライアント操作
- CRUD操作（Create, Read, Update, Delete）
- 全文検索とクエリの最適化
- 集計機能（Aggregations）
- あいまい検索（Fuzzy Search）

## 📁 プロジェクト構成

```
src/
├── client/           # ElasticSearchクライアント設定
│   ├── config.ts     # 接続設定とインデックス定義
│   └── elasticsearch.ts  # クライアントクラス
├── models/           # データモデル定義
│   └── article.ts    # 記事モデルとインターフェース
├── services/         # ビジネスロジック
│   ├── article-service.ts  # CRUD操作サービス
│   └── search-service.ts   # 検索機能サービス
└── demo/             # デモスクリプト
    ├── sample-data.ts     # サンプルデータ
    ├── crud-demo.ts       # CRUD操作デモ
    ├── search-demo.ts     # 検索機能デモ
    └── index.ts           # メインデモ
```

## 🚀 セットアップ

### 1. ElasticSearchの起動

**重要:** ディスク容量が不足している場合は、[Elastic Cloud](https://cloud.elastic.co/)の無料トライアルを利用することを強く推奨します。

ElasticSearchをDockerで起動する場合：

```bash
# Docker Composeを使用（推奨）
docker-compose up -d

# または軽量版（ディスク容量不足の場合）
docker-compose -f docker-compose.simple.yml up -d

# または個別のコンテナで起動
docker run -d \
  --name elasticsearch \
  -p 9200:9200 \
  -p 9300:9300 \
  -e "discovery.type=single-node" \
  -e "xpack.security.enabled=false" \
  elasticsearch:7.17.16
```

### 2. 依存関係のインストール

```bash
npm install
```

### 3. 環境変数の設定（オプション）

```bash
# .envファイルを作成
cp .env.example .env

# または直接環境変数を設定
export ELASTICSEARCH_URL="http://localhost:9200"
export ELASTICSEARCH_USERNAME="elastic"
export ELASTICSEARCH_PASSWORD="changeme"
```

## 📖 使い方

### メインデモの実行

```bash
npm run dev
```

### CRUD操作デモ

```bash
npm run demo:crud
```

### 検索機能デモ

```bash
npm run demo:search
```

### ビルド

```bash
npm run build
npm start
```

## 🔍 学習ポイント詳細

### 1. ElasticSearchクライアントの設定

**ファイル**: `src/client/elasticsearch.ts`

```typescript
// クライアントの初期化
const client = new Client({
  node: 'http://localhost:9200',
  auth: { username: 'elastic', password: 'changeme' }
});
```

**学習ポイント**:
- 接続設定の基本
- 認証方法
- 接続テスト（ping）の実装

### 2. インデックスとマッピングの設計

**ファイル**: `src/client/config.ts`

```typescript
mappings: {
  properties: {
    title: {
      type: 'text',
      analyzer: 'japanese_analyzer',
      fields: { keyword: { type: 'keyword' } }
    },
    // ...
  }
}
```

**学習ポイント**:
- フィールドタイプの使い分け（text vs keyword）
- アナライザーの設定
- マルチフィールドの活用

### 3. CRUD操作

**ファイル**: `src/services/article-service.ts`

**Create（作成）**:
```typescript
await client.index({
  index: indexName,
  id: article.id,
  body: article
});
```

**Read（読み取り）**:
```typescript
const response = await client.get({
  index: indexName,
  id: id
});
```

**Update（更新）**:
```typescript
await client.update({
  index: indexName,
  id: id,
  body: { doc: updates }
});
```

**Delete（削除）**:
```typescript
await client.delete({
  index: indexName,
  id: id
});
```

**学習ポイント**:
- 各操作の実装方法
- エラーハンドリング
- バルク操作の効率化

### 4. 検索クエリの構築

**ファイル**: `src/services/search-service.ts`

**基本的な全文検索**:
```typescript
{
  multi_match: {
    query: searchText,
    fields: ['title^2', 'content', 'tags'],
    type: 'best_fields',
    fuzziness: 'AUTO'
  }
}
```

**複合検索（Bool Query）**:
```typescript
{
  bool: {
    must: [/* 必須条件 */],
    filter: [/* フィルタ条件 */]
  }
}
```

**学習ポイント**:
- クエリタイプの使い分け
- スコアリングの仕組み
- フィルタとクエリの違い

### 5. 集計機能（Aggregations）

```typescript
aggs: {
  categories: {
    terms: { field: 'category', size: 10 }
  }
}
```

**学習ポイント**:
- Terms集計でのカウント
- メトリクス集計（平均、合計など）
- ネストした集計

## 💡 重要な概念

### インデックス（Index）
データベースのテーブルに相当する概念。ドキュメントの集合を格納します。

### ドキュメント（Document）
ElasticSearchに格納される個々のデータ単位。JSONフォーマットで表現されます。

### マッピング（Mapping）
フィールドのデータ型や検索方法を定義する設定。データベースのスキーマに相当します。

### アナライザー（Analyzer）
テキストを検索可能なトークンに分解する処理。日本語検索では特に重要です。

### スコアリング
検索結果の関連度を数値化する仕組み。`_score`フィールドで確認できます。

## 🛠️ 実装のベストプラクティス

### 1. エラーハンドリング
```typescript
try {
  const response = await client.search(query);
  return response.body;
} catch (error) {
  if (error.meta?.statusCode === 404) {
    // 404エラーの特別な処理
  }
  throw error;
}
```

### 2. 接続の確認
```typescript
const isConnected = await client.ping();
if (!isConnected) {
  throw new Error('ElasticSearchに接続できません');
}
```

### 3. バルク操作の活用
```typescript
const body = articles.flatMap(article => [
  { index: { _index: indexName, _id: article.id } },
  article
]);
await client.bulk({ body });
```

## 🚨 トラブルシューティング

### ディスク容量不足エラー

**エラー**: `No space left on device`

**解決方法**:
1. **Elastic Cloud を使用（推奨）**:
   - [Elastic Cloud](https://cloud.elastic.co/) で無料トライアルを開始
   - 環境変数を設定

2. **ローカルのディスク容量を確保**:
   ```bash
   # 不要なDockerイメージの削除
   docker system prune -f

   # ディスク使用量の確認
   df -h
   ```

3. **外部のElasticSearchサーバーを使用**

### 接続エラー

**解決方法**:
- ElasticSearchサーバーの起動確認
- ポート設定の確認
- 認証情報の確認

## 📚 参考リンク

### 公式ドキュメント
- [Elasticsearch Official Documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)
- [Elasticsearch JavaScript Client](https://www.elastic.co/guide/en/elasticsearch/client/javascript-api/current/index.html)
- [Elastic Cloud](https://cloud.elastic.co/)

### 学習リソース
- [Elasticsearch の基本概念](https://qiita.com/nskydiving/items/1c2dc4e0b9c98d164329)
- [Elasticsearch入門](https://knowledge.sakura.ad.jp/20693/)
- [全文検索エンジンElasticsearch入門](https://employment.en-japan.com/engineerhub/entry/2017/08/31/110000)

### チュートリアル
- [Getting started with Elasticsearch](https://www.elastic.co/guide/en/elasticsearch/reference/current/getting-started.html)
- [TypeScript Elasticsearch Tutorial](https://blog.logrocket.com/elasticsearch-query-body-builder-node-js/)

## ⚡ パフォーマンスチューニング

### インデックス設計
- 適切なシャード数の設定
- レプリカ数の最適化
- マッピングの事前定義

### クエリ最適化
- フィルタクエリの活用
- 不要なフィールドの除外
- ページネーションの実装

### 監視とメトリクス
- 検索レスポンス時間の監視
- インデックスサイズの監視
- クラスターヘルスの確認

## 🔄 次のステップ

このプロジェクトをマスターした後は、以下の発展的なトピックに挑戦してみてください：

1. **Kibanaでのデータ可視化**
2. **Logstashでのデータパイプライン構築**
3. **クラスター構成とスケーリング**
4. **セキュリティ設定の強化**
5. **Machine Learning機能の活用**

## 📄 ライセンス

MIT License