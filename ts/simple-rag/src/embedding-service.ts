import OpenAI from 'openai';
import { EmbeddingService } from './types.js';

export class OpenAIEmbeddingService implements EmbeddingService {
  private openai: OpenAI;

  constructor(apiKey: string) {
    this.openai = new OpenAI({ apiKey });
  }

  async embed(text: string): Promise<number[]> {
    try {
      const response = await this.openai.embeddings.create({
        model: 'text-embedding-3-small',
        input: text,
      });

      return response.data[0].embedding;
    } catch (error) {
      console.error('Error creating embedding:', error);
      throw new Error('Failed to create embedding');
    }
  }
}

export class MockEmbeddingService implements EmbeddingService {
  async embed(text: string): Promise<number[]> {
    const seed = this.hashCode(text);
    const random = this.seededRandom(seed);
    
    const dimension = 1536;
    const embedding = Array.from({ length: dimension }, () => random() - 0.5);
    
    const norm = Math.sqrt(embedding.reduce((sum, val) => sum + val * val, 0));
    return embedding.map(val => val / norm);
  }

  private hashCode(str: string): number {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash;
    }
    return Math.abs(hash);
  }

  private seededRandom(seed: number): () => number {
    let x = seed;
    return () => {
      x = Math.sin(x) * 10000;
      return x - Math.floor(x);
    };
  }
}