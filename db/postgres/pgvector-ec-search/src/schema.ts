import { index, integer, pgTable, real, serial, text, timestamp, vector } from "drizzle-orm/pg-core";

import { EMBEDDING_DIMENSIONS } from "./embedding.js";

export const products = pgTable(
  "products",
  {
    id: serial("id").primaryKey(),
    slug: text("slug").notNull().unique(),
    name: text("name").notNull(),
    category: text("category").notNull(),
    description: text("description").notNull(),
    tags: text("tags").array().notNull(),
    priceCents: integer("price_cents").notNull(),
    rating: real("rating").notNull(),
    popularityScore: integer("popularity_score").notNull(),
    stock: integer("stock").notNull(),
    embedding: vector("embedding", { dimensions: EMBEDDING_DIMENSIONS }).notNull(),
    createdAt: timestamp("created_at", { withTimezone: true }).notNull().defaultNow(),
    updatedAt: timestamp("updated_at", { withTimezone: true }).notNull().defaultNow(),
  },
  (table) => [
    index("products_category_idx").on(table.category),
    index("products_embedding_hnsw_idx").using("hnsw", table.embedding.op("vector_cosine_ops")),
  ],
);

export type Product = typeof products.$inferSelect;
export type NewProduct = typeof products.$inferInsert;
