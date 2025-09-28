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
          type: 'custom',
          tokenizer: 'standard',
          filter: ['lowercase', 'stop']
        }
      }
    }
  },
  mappings: {
    properties: {
      id: { type: 'keyword' },
      title: {
        type: 'text',
        analyzer: 'japanese_analyzer',
        fields: {
          keyword: { type: 'keyword' }
        }
      },
      content: {
        type: 'text',
        analyzer: 'japanese_analyzer'
      },
      author: { type: 'keyword' },
      category: { type: 'keyword' },
      tags: { type: 'keyword' },
      created_at: { type: 'date' },
      updated_at: { type: 'date' },
      view_count: { type: 'integer' },
      is_published: { type: 'boolean' }
    }
  }
};