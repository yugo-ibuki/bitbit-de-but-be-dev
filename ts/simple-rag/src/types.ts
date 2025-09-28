export interface Document {
  id: string;
  content: string;
  metadata?: Record<string, any>;
}

export interface Embedding {
  vector: number[];
  documentId: string;
}

export interface SearchResult {
  document: Document;
  similarity: number;
}

export interface VectorDatabase {
  addDocument(document: Document, embedding: number[]): Promise<void>;
  search(queryEmbedding: number[], topK?: number): Promise<SearchResult[]>;
  getDocument(id: string): Promise<Document | null>;
}

export interface EmbeddingService {
  embed(text: string): Promise<number[]>;
}