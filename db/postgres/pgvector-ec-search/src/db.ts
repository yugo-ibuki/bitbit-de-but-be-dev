import "dotenv/config";

import { drizzle } from "drizzle-orm/postgres-js";
import postgres from "postgres";

import * as schema from "./schema.js";

const connectionString = process.env.DATABASE_URL ?? "postgres://postgres:postgres@localhost:15432/pgvector_ec_search";

export const queryClient = postgres(connectionString, {
  max: 10,
});

export const db = drizzle(queryClient, { schema });
