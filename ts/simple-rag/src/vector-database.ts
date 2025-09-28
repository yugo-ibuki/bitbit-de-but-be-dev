import { Document, Embedding, SearchResult, VectorDatabase } from './types.js';

export class InMemoryVectorDatabase implements VectorDatabase {
  private documents: Map<string, Document> = new Map();
  private embeddings: Map<string, number[]> = new Map();

  async addDocument(document: Document, embedding: number[]): Promise<void> {
    this.documents.set(document.id, document);
    this.embeddings.set(document.id, embedding);
  }

  async search(queryEmbedding: number[], topK: number = 5): Promise<SearchResult[]> {
    const results: SearchResult[] = [];

    for (const [documentId, embedding] of this.embeddings.entries()) {
      const similarity = this.cosineSimilarity(queryEmbedding, embedding);
      const document = this.documents.get(documentId);
      
      if (document) {
        results.push({ document, similarity });
      }
    }

    return results
      .sort((a, b) => b.similarity - a.similarity)
      .slice(0, topK);
  }

  async getDocument(id: string): Promise<Document | null> {
    return this.documents.get(id) || null;
  }

  private cosineSimilarity(a: number[], b: number[]): number {
    if (a.length !== b.length) {
      throw new Error('Vectors must have the same length');
    }

    let dotProduct = 0;
    let normA = 0;
    let normB = 0;

    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }

    if (normA === 0 || normB === 0) {
      return 0;
    }

    return dotProduct / (Math.sqrt(normA) * Math.sqrt(normB));
  }
}