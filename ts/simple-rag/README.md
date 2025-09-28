# Simple RAG (Retrieval-Augmented Generation) Implementation

TypeScriptで実装したシンプルなRAGシステムです。

## 機能

- **ベクトルデータベース**: インメモリのベクトル検索
- **テキスト埋め込み**: OpenAI APIまたはモック埋め込みサービス
- **意味的検索**: コサイン類似度による検索
- **RAGパイプライン**: 検索結果を使った回答生成

## セットアップ

1. 依存関係のインストール:
```bash
npm install
```

2. 環境変数の設定:
```bash
cp .env.example .env
# .envファイルにOpenAI APIキーを設定（オプション）
```

3. 実行:
```bash
npm run dev
```

## 使用方法

### 基本的な使用例

```typescript
import { InMemoryVectorDatabase } from './vector-database.js';
import { OpenAIEmbeddingService } from './embedding-service.js';
import { SearchService } from './search-service.js';
import { RAGPipeline } from './rag-pipeline.js';

const vectorDb = new InMemoryVectorDatabase();
const embeddingService = new OpenAIEmbeddingService(apiKey);
const searchService = new SearchService(vectorDb, embeddingService);
const ragPipeline = new RAGPipeline(searchService, apiKey);

// ドキュメントの追加
await ragPipeline.addDocument({
  id: '1',
  content: 'TypeScriptは...',
  metadata: { category: 'programming' }
});

// 質問
const response = await ragPipeline.query('TypeScriptとは何ですか？');
console.log(response.answer);
```

## アーキテクチャ

- `types.ts`: 型定義
- `vector-database.ts`: ベクトルデータベースの実装
- `embedding-service.ts`: テキスト埋め込みサービス
- `search-service.ts`: 検索サービス
- `rag-pipeline.ts`: RAGパイプライン
- `index.ts`: デモ実行

## ライセンス

MIT