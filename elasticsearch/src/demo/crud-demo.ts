import { ArticleService } from '../services/article-service';
import { sampleArticles } from './sample-data';
import { Article } from '../models/article';

async function runCrudDemo() {
  console.log('🚀 ElasticSearch CRUD操作デモを開始します\n');

  const articleService = new ArticleService();
  const esClient = articleService.getElasticSearchClient();

  try {
    console.log('📡 ElasticSearchへの接続をテストします...');
    const isConnected = await esClient.ping();
    if (!isConnected) {
      throw new Error('ElasticSearchに接続できません');
    }

    console.log('\n🗑️  既存のインデックスを削除します...');
    await esClient.deleteIndex();

    console.log('\n✨ 新しいインデックスを作成します...');
    await esClient.createIndex();

    console.log('\n📚 サンプル記事を一括作成します...');
    await articleService.bulkCreateArticles(sampleArticles);

    console.log('\n📖 個別記事を取得してみます...');
    const article = await articleService.getArticleById('1');
    if (article) {
      console.log(`取得した記事: ${article.title}`);
      console.log(`著者: ${article.author}`);
      console.log(`カテゴリ: ${article.category}`);
    }

    console.log('\n📝 記事を更新してみます...');
    await articleService.updateArticle('1', {
      view_count: 200,
      tags: ['elasticsearch', 'search', 'database', 'updated']
    });

    console.log('\n📖 更新後の記事を確認します...');
    const updatedArticle = await articleService.getArticleById('1');
    if (updatedArticle) {
      console.log(`更新後のビュー数: ${updatedArticle.view_count}`);
      console.log(`更新後のタグ: ${updatedArticle.tags.join(', ')}`);
    }

    console.log('\n📚 全記事を取得してみます...');
    const allArticles = await articleService.getAllArticles(0, 5);
    console.log(`取得した記事数: ${allArticles.hits.hits.length}`);
    console.log(`総記事数: ${allArticles.hits.total.value}`);

    allArticles.hits.hits.forEach((hit, index) => {
      console.log(`${index + 1}. ${hit._source.title} (作成日: ${hit._source.created_at})`);
    });

    console.log('\n📝 新しい記事を作成してみます...');
    const newArticle: Article = {
      id: 'new-article-1',
      title: 'デモで作成した新しい記事',
      content: 'これはCRUDデモで作成された新しい記事です。ElasticSearchの基本操作を学習しています。',
      author: 'demo-user',
      category: 'demo',
      tags: ['demo', 'crud', 'learning'],
      created_at: new Date(),
      updated_at: new Date(),
      view_count: 0,
      is_published: true
    };

    await articleService.createArticle(newArticle);

    console.log('\n🗑️  記事を削除してみます...');
    await articleService.deleteArticle('new-article-1');

    console.log('\n🗑️  存在しない記事の削除を試してみます...');
    await articleService.deleteArticle('non-existent-id');

    console.log('\n✅ CRUD操作デモが完了しました！');

  } catch (error) {
    console.error('❌ デモ実行中にエラーが発生しました:', error);
  }
}

if (require.main === module) {
  runCrudDemo();
}