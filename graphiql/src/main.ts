import { createServer } from 'node:http';
import { createMockYoga } from './server.js';

const port = Number(process.env.PORT ?? 4000);
const yoga = createMockYoga();
const server = createServer(yoga);

server.listen(port, () => {
  console.info(`GraphQL Yoga server is running on http://localhost:${port}/graphql`);
});
