# ElasticSearch学習プロジェクト 使い方ガイド

このガイドでは、ElasticSearch学習プロジェクトの具体的な使用方法を順を追って説明します。

## 🏁 はじめに

このプロジェクトは、ElasticSearchを初めて学ぶ方向けに設計されています。実際にコードを動かしながら、ElasticSearchの基本概念と操作方法を習得できます。

## 📋 事前準備

### 1. ElasticSearchサーバーの起動

#### Docker Composeを使用する場合（推奨）

```bash
# ElasticSearchとKibanaを同時に起動
docker-compose up -d

# ログの確認
docker-compose logs -f

# 起動確認
curl http://localhost:9200

# Kibanaにアクセス（ブラウザで）
# http://localhost:5601
```

**⚠️ ディスク容量不足の場合:**
ディスク容量が不足している場合は、以下のいずれかの方法を使用してください：

1. **Elastic Cloudを使用（推奨）:**
   - [Elastic Cloud](https://cloud.elastic.co/)で無料トライアルを開始
   - 環境変数を設定：
   ```bash
   export ELASTICSEARCH_URL="https://your-deployment.es.region.aws.found.io:9243"
   export ELASTICSEARCH_USERNAME="elastic"
   export ELASTICSEARCH_PASSWORD="your-password"
   ```

2. **より軽量な設定でローカル実行:**
   ```bash
   # 軽量版のDocker Composeを使用
   docker-compose -f docker-compose.simple.yml up -d
   ```

#### 個別のDockerコンテナを使用する場合

```bash
# ElasticSearchコンテナの起動
docker run -d \
  --name elasticsearch-learning \
  -p 9200:9200 \
  -p 9300:9300 \
  -e "discovery.type=single-node" \
  -e "xpack.security.enabled=false" \
  elasticsearch:8.11.0

# 起動確認
curl http://localhost:9200
```

#### ローカルインストールの場合

ElasticSearchを直接インストールして起動してください。
- [ElasticSearch公式ダウンロードページ](https://www.elastic.co/downloads/elasticsearch)

### 2. Node.jsの環境確認

```bash
# Node.jsのバージョン確認（v16以上推奨）
node --version

# npmのバージョン確認
npm --version
```

### 3. プロジェクトのセットアップ

```bash
# 依存関係のインストール
npm install

# TypeScriptコンパイルの確認
npm run build
```

## 🚀 基本的な使い方

### Step 1: 接続テスト

最初にElasticSearchサーバーに接続できることを確認しましょう。

```bash
npm run dev
```

このコマンドを実行すると：
1. ElasticSearchへの接続テスト
2. インデックスの作成
3. サンプルデータの投入
4. 簡単な検索例の実行

が行われます。

**期待される出力例:**
```
🎯 ElasticSearch学習プロジェクトデモ

📡 ElasticSearchへの接続をテストします...
✅ ElasticSearch接続成功

🏗️  インデックスセットアップ...
✨ インデックス 'learning-elasticsearch' を作成しました

📚 サンプルデータの投入...
🚀 10件の記事を一括作成しました

🔍 簡単な検索例:
"TypeScript" での検索結果: 2件
1. TypeScriptでElasticSearchクライアントを使う
2. Node.jsでのElasticSearch活用法
```

### Step 2: CRUD操作を学ぶ

```bash
npm run demo:crud
```

このデモでは以下の操作を順番に実行します：

#### Create（作成）
- 新しい記事ドキュメントの作成
- バルク操作での複数ドキュメント作成

#### Read（読み取り）
- IDによる特定ドキュメントの取得
- 全ドキュメントの一覧取得

#### Update（更新）
- 既存ドキュメントの部分更新
- ビューカウントやタグの更新

#### Delete（削除）
- 特定ドキュメントの削除
- 存在しないドキュメントの削除処理

**学習ポイント:**
- 各操作のレスポンス内容
- エラーハンドリングの方法
- バルク操作の効率性

### Step 3: 検索機能を学ぶ

```bash
npm run demo:search
```

このデモでは多様な検索方法を体験できます：

#### 基本的な検索
- 全文検索
- キーワード検索
- スコアリングの確認

#### フィルタ検索
- カテゴリによる絞り込み
- 著者による絞り込み
- タグによる絞り込み

#### 複合検索
- 複数条件の組み合わせ
- bool クエリの活用

#### あいまい検索
- スペルミスの許容
- fuzzy検索の実装

#### 集計機能
- カテゴリ別の記事数
- 著者別の統計情報
- 平均ビュー数の算出

**学習ポイント:**
- クエリの構築方法
- スコアリングの仕組み
- 集計結果の解釈

## 🔧 カスタマイズ方法

### 新しいフィールドの追加

記事モデルに新しいフィールドを追加したい場合：

1. **モデルの更新** (`src/models/article.ts`)
```typescript
export interface Article {
  // 既存のフィールド...
  publish_date?: Date;  // 新しいフィールド
  rating?: number;      // 新しいフィールド
}
```

2. **マッピングの更新** (`src/client/config.ts`)
```typescript
mappings: {
  properties: {
    // 既存のプロパティ...
    publish_date: { type: 'date' },
    rating: { type: 'float' }
  }
}
```

3. **サンプルデータの更新** (`src/demo/sample-data.ts`)

### 新しい検索機能の追加

`src/services/search-service.ts` に新しいメソッドを追加：

```typescript
async searchByDateRange(startDate: Date, endDate: Date): Promise<SearchResult<Article>> {
  const response = await this.esClient.getClient().search({
    index: this.esClient.getIndexName(),
    body: {
      query: {
        range: {
          created_at: {
            gte: startDate,
            lte: endDate
          }
        }
      }
    }
  });

  return response.body as SearchResult<Article>;
}
```

## 🐛 よくある問題と解決方法

### 問題1: 接続エラー

**エラーメッセージ:**
```
❌ ElasticSearch接続失敗: connect ECONNREFUSED 127.0.0.1:9200
```

**解決方法:**
1. ElasticSearchサーバーが起動していることを確認
```bash
# Docker Composeの場合
docker-compose ps

# 個別コンテナの場合
docker ps | grep elasticsearch
```

2. ポートが正しく公開されていることを確認
```bash
netstat -an | grep 9200
```

### 問題2: 認証エラー

**エラーメッセージ:**
```
❌ security_exception: [security_exception] missing authentication credentials
```

**解決方法:**
環境変数を設定するか、設定ファイルを更新：

```bash
export ELASTICSEARCH_USERNAME="elastic"
export ELASTICSEARCH_PASSWORD="your_password"
```

### 問題3: インデックス作成エラー

**エラーメッセージ:**
```
❌ インデックス作成エラー: index_already_exists_exception
```

**解決方法:**
既存のインデックスを削除してから再作成：

```bash
# デモスクリプトが自動的に処理しますが、手動で削除する場合：
curl -X DELETE "http://localhost:9200/learning-elasticsearch"
```

### 問題4: 検索結果が期待と異なる

**原因:**
- データのインデックス化が完了していない
- マッピングの設定問題

**解決方法:**
```bash
# インデックス状況の確認
curl "http://localhost:9200/learning-elasticsearch/_stats"

# リフレッシュの実行（即座に検索可能にする）
curl -X POST "http://localhost:9200/learning-elasticsearch/_refresh"
```

## 📊 データの確認方法

### Kibana Dev Toolsを使用

Kibanaが利用可能な場合、以下のクエリでデータを確認できます：

```json
# 全ドキュメントの取得
GET learning-elasticsearch/_search
{
  "query": {
    "match_all": {}
  }
}

# 特定のドキュメントの取得
GET learning-elasticsearch/_doc/1

# インデックスのマッピング確認
GET learning-elasticsearch/_mapping
```

### cURLコマンドを使用

```bash
# 全ドキュメントの取得
curl "http://localhost:9200/learning-elasticsearch/_search?pretty"

# 特定のドキュメントの取得
curl "http://localhost:9200/learning-elasticsearch/_doc/1?pretty"

# インデックスの統計情報
curl "http://localhost:9200/learning-elasticsearch/_stats?pretty"
```

## 🎓 学習の進め方

### 初級レベル
1. ✅ メインデモの実行と理解
2. ✅ CRUD操作デモの実行
3. ✅ 基本的な検索デモの実行
4. ✅ コードの読解と理解

### 中級レベル
1. サンプルデータの変更と実験
2. 新しい検索機能の追加
3. マッピング設定の変更
4. パフォーマンスの測定

### 上級レベル
1. 複雑な集計クエリの作成
2. カスタムアナライザーの実装
3. エラーハンドリングの改善
4. 実際のアプリケーションへの統合

## 🔗 次のステップ

このプロジェクトをマスターしたら、以下の発展的なトピックに挑戦してください：

### 1. Kibanaでの可視化
- Kibanaのインストールと設定
- ダッシュボードの作成
- データの可視化

### 2. 本格的なアプリケーション開発
- Web APIの作成
- フロントエンドとの連携
- リアルタイム検索の実装

### 3. 運用・監視
- ログの収集と分析
- パフォーマンス監視
- バックアップとリストア

### 4. スケーリング
- クラスター構成
- シャーディング戦略
- 負荷分散

## 📞 サポート

問題や質問がある場合は：

1. READMEのトラブルシューティングセクションを確認
2. ElasticSearch公式ドキュメントを参照
3. コミュニティフォーラムで質問

**参考リンク:**
- [Elasticsearch公式ドキュメント](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)
- [Elasticsearch Discussフォーラム](https://discuss.elastic.co/)
- [Stack Overflow](https://stackoverflow.com/questions/tagged/elasticsearch)

## 🛑 コンテナの停止

学習が終わったら、リソースを節約するためにコンテナを停止しましょう：

```bash
# Docker Composeの場合
docker-compose down

# データも削除する場合
docker-compose down -v

# 個別コンテナの場合
docker stop elasticsearch-learning
docker rm elasticsearch-learning
```

Happy Learning! 🎉