import { Article } from '../models/article';

export const sampleArticles: Article[] = [
  {
    id: '1',
    title: 'ElasticSearchの基本概念',
    content: 'ElasticSearchは全文検索エンジンです。インデックス、ドキュメント、フィールドなどの基本概念を理解することが重要です。',
    author: 'yamada',
    category: 'technology',
    tags: ['elasticsearch', 'search', 'database'],
    created_at: new Date('2024-01-15'),
    updated_at: new Date('2024-01-15'),
    view_count: 150,
    is_published: true
  },
  {
    id: '2',
    title: 'TypeScriptでElasticSearchクライアントを使う',
    content: 'TypeScriptでElasticSearchの公式クライアントライブラリを使用してCRUD操作を実装する方法を学びます。',
    author: 'tanaka',
    category: 'programming',
    tags: ['typescript', 'elasticsearch', 'node.js'],
    created_at: new Date('2024-01-20'),
    updated_at: new Date('2024-01-22'),
    view_count: 89,
    is_published: true
  },
  {
    id: '3',
    title: '全文検索のクエリ最適化',
    content: 'ElasticSearchでの全文検索性能を向上させるためのクエリ最適化テクニックとベストプラクティスを紹介します。',
    author: 'sato',
    category: 'technology',
    tags: ['elasticsearch', 'performance', 'optimization'],
    created_at: new Date('2024-02-01'),
    updated_at: new Date('2024-02-01'),
    view_count: 203,
    is_published: true
  },
  {
    id: '4',
    title: 'ElasticSearchのマッピング設計',
    content: 'インデックスのマッピング設計は検索性能に大きく影響します。適切なマッピング設計の考え方を解説します。',
    author: 'yamada',
    category: 'technology',
    tags: ['elasticsearch', 'mapping', 'design'],
    created_at: new Date('2024-02-05'),
    updated_at: new Date('2024-02-06'),
    view_count: 67,
    is_published: false
  },
  {
    id: '5',
    title: 'Node.jsでのElasticSearch活用法',
    content: 'Node.jsアプリケーションでElasticSearchを効果的に活用するためのパターンとアンチパターンを紹介します。',
    author: 'tanaka',
    category: 'programming',
    tags: ['node.js', 'elasticsearch', 'backend'],
    created_at: new Date('2024-02-10'),
    updated_at: new Date('2024-02-10'),
    view_count: 134,
    is_published: true
  },
  {
    id: '6',
    title: 'ElasticSearchのセキュリティ設定',
    content: 'プロダクション環境でElasticSearchを安全に運用するためのセキュリティ設定とベストプラクティスを学びます。',
    author: 'sato',
    category: 'security',
    tags: ['elasticsearch', 'security', 'production'],
    created_at: new Date('2024-02-15'),
    updated_at: new Date('2024-02-15'),
    view_count: 98,
    is_published: true
  },
  {
    id: '7',
    title: 'ElasticSearchの集計機能入門',
    content: 'ElasticSearchの強力な集計機能（Aggregations）を使って、データの分析と可視化を行う方法を学びます。',
    author: 'kimura',
    category: 'analytics',
    tags: ['elasticsearch', 'aggregation', 'analytics'],
    created_at: new Date('2024-02-20'),
    updated_at: new Date('2024-02-20'),
    view_count: 76,
    is_published: true
  },
  {
    id: '8',
    title: 'ElasticSearchのログ管理',
    content: 'ElasticSearchを使用したログ管理システムの構築方法と、効率的なログ検索の実装について解説します。',
    author: 'yamada',
    category: 'logging',
    tags: ['elasticsearch', 'logging', 'monitoring'],
    created_at: new Date('2024-02-25'),
    updated_at: new Date('2024-02-25'),
    view_count: 112,
    is_published: true
  },
  {
    id: '9',
    title: 'ElasticSearchのスケーリング戦略',
    content: 'ElasticSearchクラスターのスケーリング戦略と、大規模データセットでの性能最適化について説明します。',
    author: 'tanaka',
    category: 'architecture',
    tags: ['elasticsearch', 'scaling', 'performance'],
    created_at: new Date('2024-03-01'),
    updated_at: new Date('2024-03-01'),
    view_count: 89,
    is_published: false
  },
  {
    id: '10',
    title: 'ElasticSearchでの日本語検索',
    content: '日本語テキストの検索に特化したElasticSearchの設定方法と、形態素解析を活用した検索精度の向上について解説します。',
    author: 'kimura',
    category: 'technology',
    tags: ['elasticsearch', 'japanese', 'nlp'],
    created_at: new Date('2024-03-05'),
    updated_at: new Date('2024-03-05'),
    view_count: 145,
    is_published: true
  }
];