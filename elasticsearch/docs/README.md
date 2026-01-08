# Elasticsearch ドキュメント

このディレクトリには、Elasticsearchの学習と実装に関する詳細なドキュメントが含まれています。

## 📚 ドキュメント一覧

### 基本概念
- [Elasticsearchのインデックス vs RDBMSのインデックス](./elasticsearch-index-vs-rdbms.md)
  - ElasticsearchとRDBMSのインデックス概念の違い
  - フィールドタイプの選択指針
  - マッピング設計のベストプラクティス

### 実装ガイド
- [プロジェクトの使い方](../how-to-use.md) - 基本的な使用方法
- [README](../README.md) - プロジェクト概要とセットアップ

## 🎯 学習の進め方

### 1. 基本概念の理解
まず、[Elasticsearchのインデックス vs RDBMSのインデックス](./elasticsearch-index-vs-rdbms.md)を読んで、Elasticsearchの基本的な概念を理解しましょう。

### 2. 実装の確認
プロジェクト内のコードを確認しながら、理論を実践で理解していきます：

```bash
# プロジェクトのセットアップ
npm install

# Elasticsearchの起動
docker-compose up -d

# デモの実行
npm run dev
```

### 3. カスタマイズ
ドキュメントの内容を参考に、自分のニーズに合わせてカスタマイズしてみましょう。

## 🔗 関連リソース

- [Elasticsearch公式ドキュメント](https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html)
- [Elasticsearch Discussフォーラム](https://discuss.elastic.co/)
- [Stack Overflow - Elasticsearch](https://stackoverflow.com/questions/tagged/elasticsearch)

## 📝 ドキュメントの更新

新しいドキュメントを追加する場合は、このREADMEも更新してください。

---

**Happy Learning! 🎉**
