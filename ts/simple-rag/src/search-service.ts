import { Document, SearchResult, VectorDatabase, EmbeddingService } from './types.js';

export class SearchService {
  constructor(
    private vectorDb: VectorDatabase,
    private embeddingService: EmbeddingService
  ) {}

  async addDocument(document: Document): Promise<void> {
    const embedding = await this.embeddingService.embed(document.content);
    await this.vectorDb.addDocument(document, embedding);
  }

  async search(query: string, topK: number = 5): Promise<SearchResult[]> {
    const queryEmbedding = await this.embeddingService.embed(query);
    return await this.vectorDb.search(queryEmbedding, topK);
  }

  async getDocument(id: string): Promise<Document | null> {
    return await this.vectorDb.getDocument(id);
  }
}