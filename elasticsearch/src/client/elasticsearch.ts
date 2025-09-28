import { Client } from '@elastic/elasticsearch';
import { elasticConfig, indexSettings } from './config';

export class ElasticSearchClient {
  private client: Client;
  private indexName: string;

  constructor() {
    this.client = new Client(elasticConfig);
    this.indexName = indexSettings.index;
  }

  async ping(): Promise<boolean> {
    try {
      const response = await this.client.ping();
      console.log('✅ ElasticSearch接続成功');
      return response.body;
    } catch (error) {
      console.error('❌ ElasticSearch接続失敗:', error);
      return false;
    }
  }

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

  async deleteIndex(): Promise<void> {
    try {
      const exists = await this.client.indices.exists({
        index: this.indexName
      });

      if (!exists.body) {
        console.log(`📋 インデックス '${this.indexName}' は存在しません`);
        return;
      }

      await this.client.indices.delete({
        index: this.indexName
      });

      console.log(`🗑️  インデックス '${this.indexName}' を削除しました`);
    } catch (error) {
      console.error('❌ インデックス削除エラー:', error);
      throw error;
    }
  }

  async getIndexInfo(): Promise<any> {
    try {
      const response = await this.client.indices.get({
        index: this.indexName
      });
      return response.body;
    } catch (error) {
      console.error('❌ インデックス情報取得エラー:', error);
      throw error;
    }
  }

  getClient(): Client {
    return this.client;
  }

  getIndexName(): string {
    return this.indexName;
  }
}