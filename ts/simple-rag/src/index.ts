import dotenv from 'dotenv';
import { InMemoryVectorDatabase } from './vector-database.js';
import { OpenAIEmbeddingService, MockEmbeddingService } from './embedding-service.js';
import { SearchService } from './search-service.js';
import { RAGPipeline, MockRAGPipeline } from './rag-pipeline.js';
import { Document } from './types.js';

dotenv.config();

async function main() {
  const apiKey = process.env.OPENAI_API_KEY;
  
  const vectorDb = new InMemoryVectorDatabase();
  
  const embeddingService = apiKey 
    ? new OpenAIEmbeddingService(apiKey)
    : new MockEmbeddingService();
  
  const searchService = new SearchService(vectorDb, embeddingService);
  
  const ragPipeline = apiKey
    ? new RAGPipeline(searchService, apiKey)
    : new MockRAGPipeline(searchService);

  const sampleDocuments: Document[] = [
    {
      id: '1',
      content: 'TypeScript is a strongly typed programming language that builds on JavaScript, giving you better tooling at any scale.',
      metadata: { category: 'programming', language: 'typescript' }
    },
    {
      id: '2',
      content: 'React is a JavaScript library for building user interfaces. It lets you compose complex UIs from small and isolated pieces of code called components.',
      metadata: { category: 'framework', library: 'react' }
    },
    {
      id: '3',
      content: 'Node.js is a JavaScript runtime built on Chrome\'s V8 JavaScript engine. Node.js uses an event-driven, non-blocking I/O model.',
      metadata: { category: 'runtime', platform: 'nodejs' }
    },
    {
      id: '4',
      content: 'Vector databases are specialized databases designed to store and query high-dimensional vectors, commonly used in machine learning applications.',
      metadata: { category: 'database', type: 'vector' }
    }
  ];

  console.log('Adding documents to the knowledge base...');
  for (const doc of sampleDocuments) {
    await ragPipeline.addDocument(doc);
    console.log(`Added document: ${doc.id}`);
  }

  console.log('\n--- RAG System Demo ---\n');

  const queries = [
    'What is TypeScript?',
    'Tell me about React',
    'How does Node.js work?',
    'What are vector databases used for?'
  ];

  for (const query of queries) {
    console.log(`Query: ${query}`);
    try {
      const response = await ragPipeline.query(query);
      console.log(`Answer: ${response.answer}`);
      console.log(`Sources found: ${response.sources.length}`);
      console.log('---');
    } catch (error) {
      console.error(`Error processing query: ${error}`);
    }
  }
}

main().catch(console.error);