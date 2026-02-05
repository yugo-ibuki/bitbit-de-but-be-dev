import { ElasticSearchClient } from '../client/elasticsearch';
import { Article, ArticleSearchQuery, SearchResult } from '../models/article';

export class SearchService {
  private esClient: ElasticSearchClient;

  constructor() {
    this.esClient = new ElasticSearchClient();
  }

  async searchArticles(searchQuery: ArticleSearchQuery): Promise<SearchResult<Article>> {
    try {
      const query = this.buildSearchQuery(searchQuery);

      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        query: query,
        sort: [
          { _score: { order: 'desc' } },
          { created_at: { order: 'desc' } }
        ],
        from: searchQuery.from || 0,
        size: searchQuery.size || 10,
        highlight: {
          fields: {
            title: {},
            content: {}
          }
        }
      });

      const totalHits = typeof response.hits.total === 'number' ? response.hits.total : response.hits.total?.value || 0;
      console.log(`🔍 検索完了: ${totalHits}件のヒット`);
      return response as SearchResult<Article>;
    } catch (error) {
      console.error('❌ 検索エラー:', error);
      throw error;
    }
  }

  async fuzzySearch(text: string, field: string = 'title'): Promise<SearchResult<Article>> {
    try {
      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        query: {
          fuzzy: {
            [field]: {
              value: text,
              fuzziness: 'AUTO'
            }
          }
        },
        highlight: {
          fields: {
            [field]: {}
          }
        }
      });

      const totalHits = typeof response.hits.total === 'number' ? response.hits.total : response.hits.total?.value || 0;
      console.log(`🔍 あいまい検索完了: ${totalHits}件のヒット`);
      return response as SearchResult<Article>;
    } catch (error) {
      console.error('❌ あいまい検索エラー:', error);
      throw error;
    }
  }

  async aggregateByCategory(): Promise<any> {
    try {
      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        size: 0,
        aggs: {
          categories: {
            terms: {
              field: 'category',
              size: 10
            }
          }
        }
      });

      console.log('📊 カテゴリ別集計完了');
      return response.aggregations;
    } catch (error) {
      console.error('❌ 集計エラー:', error);
      throw error;
    }
  }

  async aggregateByAuthor(): Promise<any> {
    try {
      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        size: 0,
        aggs: {
          authors: {
            terms: {
              field: 'author',
              size: 10
            },
            aggs: {
              avg_views: {
                avg: {
                  field: 'view_count'
                }
              }
            }
          }
        }
      });

      console.log('📊 著者別集計完了');
      return response.aggregations;
    } catch (error) {
      console.error('❌ 集計エラー:', error);
      throw error;
    }
  }

  async getPopularArticles(limit: number = 5): Promise<SearchResult<Article>> {
    try {
      const response = await this.esClient.getClient().search({
        index: this.esClient.getIndexName(),
        query: {
          bool: {
            filter: {
              term: { is_published: true }
            }
          }
        },
        sort: [
          { view_count: { order: 'desc' } }
        ],
        size: limit
      });

      console.log(`🔥 人気記事を取得: ${limit}件`);
      return response as SearchResult<Article>;
    } catch (error) {
      console.error('❌ 人気記事取得エラー:', error);
      throw error;
    }
  }

  private buildSearchQuery(searchQuery: ArticleSearchQuery): any {
    const mustClauses: any[] = [];
    const filterClauses: any[] = [];

    if (searchQuery.query) {
      mustClauses.push({
        multi_match: {
          query: searchQuery.query,
          fields: ['title^2', 'content', 'tags'],
          type: 'best_fields',
          fuzziness: 'AUTO'
        }
      });
    }

    if (searchQuery.category) {
      filterClauses.push({
        term: { category: searchQuery.category }
      });
    }

    if (searchQuery.author) {
      filterClauses.push({
        term: { author: searchQuery.author }
      });
    }

    if (searchQuery.tags && searchQuery.tags.length > 0) {
      filterClauses.push({
        terms: { tags: searchQuery.tags }
      });
    }

    if (searchQuery.published !== undefined) {
      filterClauses.push({
        term: { is_published: searchQuery.published }
      });
    }

    if (mustClauses.length === 0 && filterClauses.length === 0) {
      return { match_all: {} };
    }

    return {
      bool: {
        must: mustClauses,
        filter: filterClauses
      }
    };
  }
}