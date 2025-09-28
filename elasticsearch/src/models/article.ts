export interface Article {
  id: string;
  title: string;
  content: string;
  author: string;
  category: string;
  tags: string[];
  created_at: Date;
  updated_at: Date;
  view_count: number;
  is_published: boolean;
}

export interface ArticleSearchQuery {
  query?: string;
  category?: string;
  author?: string;
  tags?: string[];
  published?: boolean;
  from?: number;
  size?: number;
}

export interface SearchResult<T> {
  hits: {
    total: {
      value: number;
      relation: string;
    };
    hits: Array<{
      _source: T;
      _score: number;
      _id: string;
    }>;
  };
  took: number;
}