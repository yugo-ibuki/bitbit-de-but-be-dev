# Elasticsearchのインデックス vs RDBMSのインデックス

## 📋 概要

ElasticsearchとRDBMSのインデックスは、名前は似ていますが**全く異なる概念**です。このドキュメントでは、両者の違いとElasticsearchでのインデックス設計のベストプラクティスを説明します。

## 🔍 基本概念の比較

### Elasticsearchのインデックス
- **役割**: データベースのテーブルに相当
- **目的**: データ構造の定義 + 検索最適化
- **作成タイミング**: データ投入前（必須）
- **定義内容**: フィールドタイプ、アナライザー、マッピング

### RDBMSのインデックス
- **役割**: クエリ性能向上の仕組み
- **目的**: 検索・ソートの高速化
- **作成タイミング**: データ投入後（任意）
- **定義内容**: カラム、ソート順、一意性制約

## 📊 詳細比較表

| 項目 | Elasticsearch | RDBMS |
|------|---------------|-------|
| **概念レベル** | テーブル相当 | テーブル内の仕組み |
| **必須性** | 必須（データ投入前に作成） | 任意（後から追加可能） |
| **主な目的** | データ構造定義 + 検索最適化 | クエリ性能向上 |
| **定義内容** | フィールドタイプ、アナライザー | カラム、ソート順 |
| **検索機能** | 全文検索、あいまい検索、集計 | 完全一致、範囲検索 |
| **スキーマ** | 動的（後からフィールド追加可能） | 静的（スキーマ変更が必要） |

## 🏗️ Elasticsearchインデックスの構成要素

### 1. インデックス設定（Settings）

```typescript
settings: {
  number_of_shards: 1,        // シャード数
  number_of_replicas: 0,      // レプリカ数
  analysis: {                 // アナライザー設定
    analyzer: {
      japanese_analyzer: {
        type: 'custom',
        tokenizer: 'standard',
        filter: ['lowercase', 'stop']
      }
    }
  }
}
```

### 2. マッピング（Mappings）

```typescript
mappings: {
  properties: {
    // テキスト検索用
    title: {
      type: 'text',
      analyzer: 'japanese_analyzer',
      fields: {
        keyword: { type: 'keyword' }  // 完全一致・ソート用
      }
    },
    
    // 完全一致検索用
    author: { type: 'keyword' },
    category: { type: 'keyword' },
    
    // 数値型
    view_count: { type: 'integer' },
    
    // 日付型
    created_at: { type: 'date' },
    
    // ブール型
    is_published: { type: 'boolean' }
  }
}
```

## 🎯 フィールドタイプの選択指針

### Text vs Keyword

| タイプ | 用途 | 検索方法 | 例 |
|--------|------|----------|-----|
| `text` | 全文検索 | 部分一致、あいまい検索 | 記事のタイトル、内容 |
| `keyword` | 完全一致 | 完全一致、ソート、集計 | カテゴリ、タグ、ID |

### マルチフィールドの活用

```typescript
title: {
  type: 'text',                    // 全文検索用
  analyzer: 'japanese_analyzer',
  fields: {
    keyword: { type: 'keyword' },  // 完全一致・ソート用
    raw: { type: 'text' }          // 生データ検索用
  }
}
```

**使用例:**
- `title` - 全文検索（"TypeScript"で部分一致）
- `title.keyword` - 完全一致・ソート（"TypeScript入門"で完全一致）
- `title.raw` - 生データ検索（大文字小文字区別）

## 🔧 実装例

### インデックス作成

```typescript
// src/client/elasticsearch.ts
async createIndex(): Promise<void> {
  try {
    const exists = await this.client.indices.exists({
      index: this.indexName
    });

    if (exists.body) {
      console.log(`📋 インデックス '${this.indexName}' は既に存在します`);
      return;
    }

    await this.client.indices.create({
      index: this.indexName,
      body: {
        settings: indexSettings.settings,
        mappings: indexSettings.mappings
      }
    });

    console.log(`✨ インデックス '${this.indexName}' を作成しました`);
  } catch (error) {
    console.error('❌ インデックス作成エラー:', error);
    throw error;
  }
}
```

### 設定ファイル

```typescript
// src/client/config.ts
export const indexSettings = {
  index: 'learning-elasticsearch',
  settings: {
    number_of_shards: 1,
    number_of_replicas: 0,
    analysis: {
      analyzer: {
        japanese_analyzer: {
          type: 'custom',
          tokenizer: 'standard',
          filter: ['lowercase', 'stop']
        }
      }
    }
  },
  mappings: {
    properties: {
      id: { type: 'keyword' },
      title: {
        type: 'text',
        analyzer: 'japanese_analyzer',
        fields: {
          keyword: { type: 'keyword' }
        }
      },
      content: {
        type: 'text',
        analyzer: 'japanese_analyzer'
      },
      author: { type: 'keyword' },
      category: { type: 'keyword' },
      tags: { type: 'keyword' },
      created_at: { type: 'date' },
      updated_at: { type: 'date' },
      view_count: { type: 'integer' },
      is_published: { type: 'boolean' }
    }
  }
};
```

## 🚀 ベストプラクティス

### 1. フィールドタイプの選択

```typescript
// ✅ 良い例
title: { type: 'text' }        // 全文検索が必要
author: { type: 'keyword' }    // 完全一致のみ
view_count: { type: 'integer' } // 数値計算が必要

// ❌ 避けるべき例
title: { type: 'keyword' }     // 全文検索ができない
author: { type: 'text' }      // 不要なトークン化
```

### 2. マルチフィールドの活用

```typescript
// 検索とソートの両方に対応
title: {
  type: 'text',
  fields: {
    keyword: { type: 'keyword' }  // ソート・集計用
  }
}
```

### 3. アナライザーの選択

```typescript
// 日本語検索用
analyzer: 'japanese_analyzer'

// 英語検索用
analyzer: 'standard'

// カスタムアナライザー
analyzer: 'custom_analyzer'
```

### 4. 動的マッピングの制御

```typescript
mappings: {
  dynamic: 'strict',  // 未定義フィールドを拒否
  properties: {
    // 明示的に定義
  }
}
```

## 🔍 検索クエリでの活用

### Text フィールドでの検索

```typescript
// 全文検索
{
  query: {
    match: {
      title: "TypeScript"
    }
  }
}

// あいまい検索
{
  query: {
    match: {
      title: {
        query: "TypeScript",
        fuzziness: "AUTO"
      }
    }
  }
}
```

### Keyword フィールドでの検索

```typescript
// 完全一致
{
  query: {
    term: {
      category: "プログラミング"
    }
  }
}

// 複数値の完全一致
{
  query: {
    terms: {
      tags: ["TypeScript", "JavaScript"]
    }
  }
}
```

### マルチフィールドでの検索

```typescript
// ソート
{
  query: { match_all: {} },
  sort: [
    { "title.keyword": "asc" }
  ]
}

// 集計
{
  aggs: {
    categories: {
      terms: {
        field: "category.keyword"
      }
    }
  }
}
```

## ⚠️ 注意点

### 1. マッピングの変更制限

- 既存フィールドのタイプ変更は不可
- 新しいフィールドの追加は可能
- 必要に応じてインデックス再作成

### 2. パフォーマンス考慮

```typescript
// シャード数の設定
number_of_shards: 1,        // 小規模データ
number_of_shards: 3,        // 中規模データ
number_of_shards: 5,        // 大規模データ
```

### 3. メモリ使用量

- `text` フィールドはメモリを多く使用
- `keyword` フィールドは比較的軽量
- 不要なフィールドは `index: false` を設定

## 📚 関連ドキュメント

- [Elasticsearch公式ドキュメント - Mapping](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html)
- [Elasticsearch公式ドキュメント - Field Data Types](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping-types.html)
- [プロジェクト内の実装例](../src/client/config.ts)

## 🎯 まとめ

Elasticsearchのインデックスは：

1. **RDBMSのテーブルに相当**する概念
2. **データ投入前に必須**で作成する
3. **フィールドタイプとアナライザー**で検索性能を決定
4. **マルチフィールド**で多様な検索ニーズに対応
5. **設計が検索性能に直結**する

適切なインデックス設計により、高速で柔軟な検索機能を実現できます。
