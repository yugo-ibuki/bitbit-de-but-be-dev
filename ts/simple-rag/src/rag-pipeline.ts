import OpenAI from 'openai';
import { Document, SearchResult } from './types.js';
import { SearchService } from './search-service.js';

export interface RAGResponse {
  answer: string;
  sources: SearchResult[];
}

export class RAGPipeline {
  private openai: OpenAI;

  constructor(
    private searchService: SearchService,
    apiKey: string
  ) {
    this.openai = new OpenAI({ apiKey });
  }

  async query(question: string, topK: number = 3): Promise<RAGResponse> {
    const searchResults = await this.searchService.search(question, topK);
    
    const context = searchResults
      .map(result => result.document.content)
      .join('\n\n');

    const systemPrompt = `You are a helpful assistant that answers questions based on the provided context. Use only the information from the context to answer the question. If the context doesn't contain enough information to answer the question, say so.

Context:
${context}`;

    const userPrompt = `Question: ${question}`;

    try {
      const response = await this.openai.chat.completions.create({
        model: 'gpt-3.5-turbo',
        messages: [
          { role: 'system', content: systemPrompt },
          { role: 'user', content: userPrompt }
        ],
        temperature: 0.7,
        max_tokens: 500
      });

      const answer = response.choices[0]?.message?.content || 'No answer generated.';

      return {
        answer,
        sources: searchResults
      };
    } catch (error) {
      console.error('Error generating answer:', error);
      throw new Error('Failed to generate answer');
    }
  }

  async addDocument(document: Document): Promise<void> {
    await this.searchService.addDocument(document);
  }
}

export class MockRAGPipeline {
  constructor(private searchService: SearchService) {}

  async query(question: string, topK: number = 3): Promise<RAGResponse> {
    const searchResults = await this.searchService.search(question, topK);
    
    const answer = `Based on the search results for "${question}", here's what I found: ${
      searchResults.length > 0 
        ? `The most relevant information suggests ${searchResults[0].document.content.substring(0, 100)}...`
        : 'No relevant information found in the knowledge base.'
    }`;

    return {
      answer,
      sources: searchResults
    };
  }

  async addDocument(document: Document): Promise<void> {
    await this.searchService.addDocument(document);
  }
}