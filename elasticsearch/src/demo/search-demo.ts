import { ArticleService } from '../services/article-service';
import { SearchService } from '../services/search-service';
import { sampleArticles } from './sample-data';

async function runSearchDemo() {
  console.log('🔍 ElasticSearch検索機能デモを開始します\n');

  const articleService = new ArticleService();
  const searchService = new SearchService();
  const esClient = articleService.getElasticSearchClient();

  try {
    console.log('📡 ElasticSearchへの接続をテストします...');
    const isConnected = await esClient.ping();
    if (!isConnected) {
      throw new Error('ElasticSearchに接続できません');
    }

    console.log('\n🗑️  既存のインデックスを削除して新しくします...');
    await esClient.deleteIndex();
    await esClient.createIndex();

    console.log('\n📚 サンプル記事を一括作成します...');
    await articleService.bulkCreateArticles(sampleArticles);

    await new Promise(resolve => setTimeout(resolve, 1000));

    console.log('\n🔍 1. 基本的な全文検索');
    console.log('検索キーワード: "ElasticSearch"');
    const basicSearch = await searchService.searchArticles({
      query: 'ElasticSearch'
    });
    console.log(`ヒット数: ${basicSearch.hits.total.value}`);
    basicSearch.hits.hits.slice(0, 3).forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (スコア: ${hit._score?.toFixed(2)})`);
    });

    console.log('\n🔍 2. カテゴリ絞り込み検索');
    console.log('検索条件: technology カテゴリの記事');
    const categorySearch = await searchService.searchArticles({
      category: 'technology'
    });
    console.log(`ヒット数: ${categorySearch.hits.total.value}`);
    categorySearch.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (カテゴリ: ${hit._source.category})`);
    });

    console.log('\n🔍 3. 著者絞り込み検索');
    console.log('検索条件: yamada さんの記事');
    const authorSearch = await searchService.searchArticles({
      author: 'yamada'
    });
    console.log(`ヒット数: ${authorSearch.hits.total.value}`);
    authorSearch.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (著者: ${hit._source.author})`);
    });

    console.log('\n🔍 4. タグ検索');
    console.log('検索条件: "elasticsearch" と "performance" タグを含む記事');
    const tagSearch = await searchService.searchArticles({
      tags: ['elasticsearch', 'performance']
    });
    console.log(`ヒット数: ${tagSearch.hits.total.value}`);
    tagSearch.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (タグ: ${hit._source.tags.join(', ')})`);
    });

    console.log('\n🔍 5. 複合検索（キーワード + カテゴリ + 公開状態）');
    console.log('検索条件: "TypeScript" を含む technology カテゴリの公開記事');
    const complexSearch = await searchService.searchArticles({
      query: 'TypeScript',
      category: 'technology',
      published: true
    });
    console.log(`ヒット数: ${complexSearch.hits.total.value}`);
    complexSearch.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (カテゴリ: ${hit._source.category}, 公開: ${hit._source.is_published})`);
    });

    console.log('\n🔍 6. あいまい検索');
    console.log('検索キーワード: "ElasticSeach" (スペルミス)');
    const fuzzySearch = await searchService.fuzzySearch('ElasticSeach');
    console.log(`ヒット数: ${fuzzySearch.hits.total.value}`);
    fuzzySearch.hits.hits.slice(0, 3).forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title}`);
    });

    console.log('\n🔥 7. 人気記事の取得');
    const popularArticles = await searchService.getPopularArticles(3);
    console.log(`人気記事トップ3:`);
    popularArticles.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (ビュー数: ${hit._source.view_count})`);
    });

    console.log('\n📊 8. カテゴリ別集計');
    const categoryAgg = await searchService.aggregateByCategory();
    console.log('カテゴリ別記事数:');
    categoryAgg.categories.buckets.forEach((bucket: any) => {
      console.log(`- ${bucket.key}: ${bucket.doc_count}件`);
    });

    console.log('\n📊 9. 著者別集計（平均ビュー数付き）');
    const authorAgg = await searchService.aggregateByAuthor();
    console.log('著者別記事数と平均ビュー数:');
    authorAgg.authors.buckets.forEach((bucket: any) => {
      console.log(`- ${bucket.key}: ${bucket.doc_count}件, 平均ビュー数: ${bucket.avg_views.value?.toFixed(1) || 0}`);
    });

    console.log('\n✅ 検索機能デモが完了しました！');

  } catch (error) {
    console.error('❌ デモ実行中にエラーが発生しました:', error);
  }
}

if (require.main === module) {
  runSearchDemo();
}