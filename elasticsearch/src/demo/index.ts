import { ArticleService } from '../services/article-service';
import { SearchService } from '../services/search-service';
import { sampleArticles } from './sample-data';

async function main() {
  console.log('🎯 ElasticSearch学習プロジェクトデモ\n');
  console.log('このプロジェクトでは以下の機能を学習できます:');
  console.log('1. ElasticSearchクライアントの接続');
  console.log('2. インデックスの作成・削除');
  console.log('3. ドキュメントのCRUD操作');
  console.log('4. 全文検索・複合検索');
  console.log('5. 集計機能（Aggregations）');
  console.log('6. あいまい検索（Fuzzy Search）\n');

  console.log('利用可能なデモスクリプト:');
  console.log('- npm run demo:crud   : CRUD操作のデモ');
  console.log('- npm run demo:search : 検索機能のデモ');
  console.log('- npm run dev         : このメインデモ\n');

  const articleService = new ArticleService();
  const searchService = new SearchService();
  const esClient = articleService.getElasticSearchClient();

  try {
    console.log('📡 ElasticSearchへの接続をテストします...');
    const isConnected = await esClient.ping();
    if (!isConnected) {
      console.error('❌ ElasticSearchに接続できません。');
      console.log('\n💡 解決方法:');
      console.log('1. ElasticSearchが起動していることを確認してください');
      console.log('2. 接続情報（URL、認証情報）が正しいことを確認してください');
      console.log('3. 環境変数 ELASTICSEARCH_URL, ELASTICSEARCH_USERNAME, ELASTICSEARCH_PASSWORD を設定してください');
      return;
    }

    console.log('\n🏗️  インデックスセットアップ...');
    await esClient.deleteIndex();
    await esClient.createIndex();

    console.log('\n📚 サンプルデータの投入...');
    await articleService.bulkCreateArticles(sampleArticles);

    await new Promise(resolve => setTimeout(resolve, 1000));

    console.log('\n🔍 簡単な検索例:');

    const searchResult = await searchService.searchArticles({
      query: 'TypeScript',
      size: 3
    });

    console.log(`"TypeScript" での検索結果: ${searchResult.hits.total.value}件`);
    searchResult.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title}`);
    });

    console.log('\n📊 カテゴリ別統計:');
    const categoryStats = await searchService.aggregateByCategory();
    categoryStats.categories.buckets.forEach((bucket: any) => {
      console.log(`- ${bucket.key}: ${bucket.doc_count}件`);
    });

    console.log('\n✅ デモ完了！詳細なデモは以下のコマンドで実行できます:');
    console.log('- npm run demo:crud   : CRUD操作の詳細デモ');
    console.log('- npm run demo:search : 検索機能の詳細デモ');

  } catch (error) {
    console.error('❌ エラーが発生しました:', error);
    console.log('\n💡 トラブルシューティング:');
    console.log('1. ElasticSearchサーバーが起動していることを確認');
    console.log('2. ネットワーク接続を確認');
    console.log('3. 認証情報が正しいことを確認');
  }
}

if (require.main === module) {
  main();
}