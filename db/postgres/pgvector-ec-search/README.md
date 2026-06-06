# PG Vector EC Search

PG Vector を学ぶための、EC 商品検索バックエンドのベースラインです。

商品データを Postgres に保存し、`pgvector` の `vector(12)` と HNSW index で cosine distance の近傍検索を行います。検索 API は、まずベクトル距離で候補を取り、その後に人気度・評価・価格フィットをアプリ側で重み付けして並べ替えます。

## Stack

- Postgres 17 + `pgvector/pgvector`
- TypeScript
- Hono
- Drizzle ORM / Drizzle Kit
- Vitest

## Setup

```bash
cd db/postgres/pgvector-ec-search
npm install
cp .env.example .env
docker compose up -d
npm run db:migrate
npm run seed
npm run dev
```

API はデフォルトで `http://localhost:8787` に起動します。

Docker の Postgres は `localhost:15432` で公開します。既存のローカル Postgres と衝突しにくいように、ホスト側の `5432` は使っていません。

## Commands

```bash
npm test          # 埋め込み生成と重み付きランキングのテスト
npm run build    # TypeScript build
docker compose up -d
docker compose down
npm run db:up    # docker compose up -d の npm script
npm run db:down  # pgvector Postgres を停止
npm run db:reset # volume ごと作り直し
npm run db:migrate
npm run seed
npm run dev
```

## Try The API

Health check:

```bash
curl -s http://localhost:8787/health | jq
```

Seed 済み商品一覧:

```bash
curl -s http://localhost:8787/products | jq
```

意味検索を強める例:

```bash
curl -X POST http://localhost:8787/search \
  -H 'content-type: application/json' \
  -d '{
    "query": "waterproof running shoes for outdoor trails",
    "limit": 5,
    "weights": {
      "semantic": 0.85,
      "popularity": 0.05,
      "rating": 0.05,
      "priceFit": 0.05
    }
  }' | jq
```

価格フィットを強める例:

```bash
curl -X POST http://localhost:8787/search \
  -H 'content-type: application/json' \
  -d '{
    "query": "waterproof running shoes for outdoor trails",
    "targetPriceCents": 9000,
    "limit": 5,
    "weights": {
      "semantic": 0.35,
      "popularity": 0.05,
      "rating": 0.05,
      "priceFit": 0.55
    }
  }' | jq
```

カテゴリと価格帯で絞る例:

```bash
curl -X POST http://localhost:8787/search \
  -H 'content-type: application/json' \
  -d '{
    "query": "home kitchen baking tool",
    "category": "kitchen",
    "minPriceCents": 5000,
    "maxPriceCents": 35000,
    "limit": 3
  }' | jq
```

重み付けの意味が分かるように、説明付きで見る例:

```bash
curl -s -X POST http://localhost:8787/search \
  -H 'content-type: application/json' \
  -d '{
    "query": "waterproof running shoes for outdoor trails",
    "limit": 5,
    "weights": {
      "semantic": 0.85,
      "popularity": 0.05,
      "rating": 0.05,
      "priceFit": 0.05
    }
  }' | jq '{
    what_changed: "semantic を 0.85 にして、ベクトル検索の近さを最も重視した",
    how_to_read: "cosineDistance が小さいほど query に近く、weightedScore が最終順位",
    results: [
      .results[] | {
        name: .product.name,
        category: .product.category,
        priceCents: .product.priceCents,
        rating: .product.rating,
        popularityScore: .product.popularityScore,
        cosineDistance,
        semanticSimilarity,
        weightedScore
      }
    ]
  }'
```

同じ query で、価格フィットを強めた場合と比較する例:

```bash
curl -s -X POST http://localhost:8787/search \
  -H 'content-type: application/json' \
  -d '{
    "query": "waterproof running shoes for outdoor trails",
    "targetPriceCents": 9000,
    "limit": 5,
    "weights": {
      "semantic": 0.35,
      "popularity": 0.05,
      "rating": 0.05,
      "priceFit": 0.55
    }
  }' | jq '{
    what_changed: "targetPriceCents を 9000 にして priceFit を 0.55 にした",
    how_to_read: "query との近さだけでなく、9000円に近い商品ほど weightedScore が上がる",
    results: [
      .results[] | {
        name: .product.name,
        priceCents: .product.priceCents,
        distanceFromTargetPrice: (.product.priceCents - 9000 | if . < 0 then -. else . end),
        cosineDistance,
        semanticSimilarity,
        weightedScore
      }
    ]
  }'
```

## Files

- `src/schema.ts`: Drizzle schema。`embedding vector(12)` と HNSW index を定義
- `src/embedding.ts`: 外部 API 不要の決定的なローカル埋め込み関数
- `src/seed-data.ts`: EC 商品の seed データ
- `src/search.ts`: `cosineDistance` で候補を取り、重み付きスコアで並べ替える検索処理
- `src/server.ts`: Hono API
- `drizzle/`: Drizzle Kit が生成した migration
- `docker/init/001-enable-vector.sql`: pgvector 拡張の有効化

## Notes

アプリケーション側の DB 操作は Drizzle API で書いています。`CREATE EXTENSION IF NOT EXISTS vector` だけは Postgres 側で拡張を有効にするために必要なので、Docker 初期化ファイルに閉じています。

`src/embedding.ts` は学習用のローカル埋め込みです。実アプリでは OpenAI embeddings などに差し替え、`EMBEDDING_DIMENSIONS` と `schema.ts` の `vector` 次元を合わせてください。
