import { createSchema, createYoga } from 'graphql-yoga';

type Post = {
  id: string;
  title: string;
  body: string;
  tags: string[];
  published: boolean;
};

type CreatePostInput = {
  title: string;
  body: string;
  tags?: string[] | null;
};

const initialPosts = (): Post[] => [
  {
    id: 'post-1',
    title: 'Codex App Browser Use と GraphiQL の相性',
    body: 'GraphiQL はブラウザ上でクエリを書けるため、Codex App の browser use で操作手順を見せやすい。',
    tags: ['codex', 'graphql', 'graphiql'],
    published: true,
  },
  {
    id: 'post-2',
    title: 'Yoga で作る軽量 GraphQL モック',
    body: '記事用の画面を出すだけなら、インメモリデータと小さなスキーマで十分に使える。',
    tags: ['yoga', 'mock'],
    published: true,
  },
];

export const defaultGraphiQLQuery = /* GraphQL */ `
  query ArticleDemo {
    viewer {
      id
      name
      role
    }
    posts {
      id
      title
      tags
      published
    }
  }
`;

export const createMockYoga = () => {
  const posts = initialPosts();

  return createYoga({
    graphqlEndpoint: '/graphql',
    graphiql: {
      title: 'Codex App x GraphiQL Mock',
      defaultQuery: defaultGraphiQLQuery,
    },
    schema: createSchema({
      typeDefs: /* GraphQL */ `
        type Viewer {
          id: ID!
          name: String!
          role: String!
        }

        type Post {
          id: ID!
          title: String!
          body: String!
          tags: [String!]!
          published: Boolean!
        }

        input CreatePostInput {
          title: String!
          body: String!
          tags: [String!]
        }

        type Query {
          viewer: Viewer!
          posts: [Post!]!
          post(id: ID!): Post
          searchPosts(keyword: String!): [Post!]!
        }

        type Mutation {
          createPost(input: CreatePostInput!): Post!
          publishPost(id: ID!): Post
        }
      `,
      resolvers: {
        Query: {
          viewer: () => ({
            id: 'viewer-1',
            name: 'Codex Writer',
            role: 'Article author',
          }),
          posts: () => posts,
          post: (_parent: unknown, args: { id: string }) =>
            posts.find((post) => post.id === args.id) ?? null,
          searchPosts: (_parent: unknown, args: { keyword: string }) => {
            const keyword = args.keyword.trim().toLowerCase();

            return posts.filter((post) => {
              const searchableText = [
                post.title,
                post.body,
                ...post.tags,
              ].join(' ');

              return searchableText.toLowerCase().includes(keyword);
            });
          },
        },
        Mutation: {
          createPost: (_parent: unknown, args: { input: CreatePostInput }) => {
            const post: Post = {
              id: `post-${posts.length + 1}`,
              title: args.input.title,
              body: args.input.body,
              tags: args.input.tags ?? [],
              published: false,
            };

            posts.push(post);

            return post;
          },
          publishPost: (_parent: unknown, args: { id: string }) => {
            const post = posts.find((candidate) => candidate.id === args.id);

            if (!post) {
              return null;
            }

            post.published = true;

            return post;
          },
        },
      },
    }),
  });
};
