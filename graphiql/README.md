# GraphiQL Yoga Mock

Codex App の browser use と GraphiQL の相性を紹介する記事用の、軽量な GraphQL Yoga モックサーバーです。

## Setup

```bash
npm install
```

## Development

```bash
npm run dev
```

Open http://localhost:4000/graphql to use GraphiQL.

## Useful Queries

```graphql
query ArticleDemo {
  viewer {
    id
    name
    role
  }
  posts {
    id
    title
    tags
    published
  }
}
```

```graphql
mutation CreatePost {
  createPost(
    input: {
      title: "Browser Use で GraphQL を触る"
      body: "GraphiQL の画面を Codex App から操作する記事用のメモ。"
      tags: ["browser-use"]
    }
  ) {
    id
    title
    published
  }
}
```
