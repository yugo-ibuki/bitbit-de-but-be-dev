import { describe, expect, test } from 'vitest';
import { createMockYoga } from '../src/server.js';

const executeGraphQL = async (query: string, variables?: Record<string, unknown>) => {
  const yoga = createMockYoga();
  const response = await yoga.fetch('http://localhost:4000/graphql', {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
    },
    body: JSON.stringify({ query, variables }),
  });

  return response.json() as Promise<{
    data?: unknown;
    errors?: Array<{ message: string }>;
  }>;
};

describe('GraphiQL mock GraphQL server', () => {
  test('returns viewer and post data for the default GraphiQL query', async () => {
    const result = await executeGraphQL(/* GraphQL */ `
      query ArticleDemo {
        viewer {
          name
          role
        }
        posts {
          id
          title
          tags
        }
      }
    `);

    expect(result.errors).toBeUndefined();
    expect(result.data).toMatchObject({
      viewer: {
        name: 'Codex Writer',
        role: 'Article author',
      },
      posts: expect.arrayContaining([
        {
          id: 'post-1',
          title: 'Codex App Browser Use と GraphiQL の相性',
          tags: ['codex', 'graphql', 'graphiql'],
        },
      ]),
    });
  });

  test('creates an in-memory post through a mutation', async () => {
    const result = await executeGraphQL(
      /* GraphQL */ `
        mutation CreatePost($input: CreatePostInput!) {
          createPost(input: $input) {
            id
            title
            published
          }
        }
      `,
      {
        input: {
          title: 'Browser Use で GraphQL を触る',
          body: 'GraphiQL の画面を Codex App から操作する記事用のメモ。',
          tags: ['browser-use'],
        },
      },
    );

    expect(result.errors).toBeUndefined();
    expect(result.data).toMatchObject({
      createPost: {
        id: 'post-3',
        title: 'Browser Use で GraphQL を触る',
        published: false,
      },
    });
  });
});
