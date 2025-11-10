export const elasticConfig = {
  node: process.env.ELASTICSEARCH_URL || 'http://localhost:9200',
  auth: {
    username: process.env.ELASTICSEARCH_USERNAME || 'elastic',
    password: process.env.ELASTICSEARCH_PASSWORD || 'changeme'
  },
  tls: {
    rejectUnauthorized: false
  }
};

export const indexSettings = {
  index: 'learning-elasticsearch',
  settings: {
    number_of_shards: 1,
    number_of_replicas: 0,
    analysis: {
      analyzer: {
        japanese_analyzer: {
          type: 'custom' as const,
          tokenizer: 'standard',
          filter: ['lowercase', 'stop']
        }
      }
    }
  },
  mappings: {
    properties: {
      id: { type: 'keyword' as const },
      title: {
        type: 'text' as const,
        analyzer: 'japanese_analyzer',
        fields: {
          keyword: { type: 'keyword' as const }
        }
      },
      content: {
        type: 'text' as const,
        analyzer: 'japanese_analyzer'
      },
      author: { type: 'keyword' as const },
      category: { type: 'keyword' as const },
      tags: { type: 'keyword' as const },
      created_at: { type: 'date' as const },
      updated_at: { type: 'date' as const },
      view_count: { type: 'integer' as const },
      is_published: { type: 'boolean' as const }
    }
  }
};