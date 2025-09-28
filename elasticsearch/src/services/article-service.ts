import { ElasticSearchClient } from '../client/elasticsearch';
import { Article, ArticleSearchQuery, SearchResult } from '../models/article';

export class ArticleService {
  private esClient: ElasticSearchClient;

  constructor() {
    this.esClient = new ElasticSearchClient();
  }

  async createArticle(article: Article): Promise<void> {
    try {
      const response = await this.esClient.getClient().index({
        index: this.esClient.getIndexName(),
        id: article.id,
        body: article
      });

      console.log(`✅ 記事を作成しました: ${article.title} (ID: ${article.id})`);
      console.log(`📊 結果: ${response.body.result}`);
    } catch (error) {
      console.error('❌ 記事作成エラー:', error);
      throw error;
    }
  }

  async getArticleById(id: string): Promise<Article | null> {
    try {
      const response = await this.esClient.getClient().get({
        index: this.esClient.getIndexName(),
        id: id
      });

      console.log(`📖 記事を取得しました: ID ${id}`);
      return response.body._source as Article;
    } catch (error: any) {
      if (error.meta?.statusCode === 404) {
        console.log(`📭 記事が見つかりません: ID ${id}`);
        return null;
      }
      console.error('❌ 記事取得エラー:', error);
      throw error;
    }
  }

  async updateArticle(id: string, updates: Partial<Article>): Promise<void> {
    try {
      updates.updated_at = new Date();

      const response = await this.esClient.getClient().update({
        index: this.esClient.getIndexName(),
        id: id,
        body: {
          doc: updates
        }
      });

      console.log(`✏️  記事を更新しました: ID ${id}`);
      console.log(`📊 結果: ${response.body.result}`);
    } catch (error) {
      console.error('❌ 記事更新エラー:', error);
      throw error;
    }
  }

  async deleteArticle(id: string): Promise<void> {
    try {
      const response = await this.esClient.getClient().delete({
        index: this.esClient.getIndexName(),
        id: id
      });

      console.log(`🗑️  記事を削除しました: ID ${id}`);
      console.log(`📊 結果: ${response.body.result}`);
    } catch (error: any) {
      if (error.meta?.statusCode === 404) {
        console.log(`📭 削除対象の記事が見つかりません: ID ${id}`);
        return;
      }
      console.error('❌ 記事削除エラー:', error);
      throw error;
    }
  }

  async getAllArticles(from: number = 0, size: number = 10): Promise<SearchResult<Article>> {
    try {
      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        body: {
          query: {
            match_all: {}
          },
          sort: [
            { created_at: { order: 'desc' } }
          ],
          from: from,
          size: size
        }
      });

      console.log(`📚 全記事を取得しました (${from}-${from + size})`);
      return response.body as SearchResult<Article>;
    } catch (error) {
      console.error('❌ 全記事取得エラー:', error);
      throw error;
    }
  }

  async bulkCreateArticles(articles: Article[]): Promise<void> {
    try {
      const body = articles.flatMap(article => [
        { index: { _index: this.esClient.getIndexName(), _id: article.id } },
        article
      ]);

      const response = await this.esClient.getClient().bulk({ body });

      const errors = response.body.items.filter((item: any) =>
        item.index && item.index.error
      );

      if (errors.length > 0) {
        console.error('❌ 一部の記事作成に失敗:', errors);
      }

      console.log(`🚀 ${articles.length}件の記事を一括作成しました`);
      console.log(`⏱️  処理時間: ${response.body.took}ms`);
    } catch (error) {
      console.error('❌ 記事一括作成エラー:', error);
      throw error;
    }
  }

  getElasticSearchClient(): ElasticSearchClient {
    return this.esClient;
  }
}