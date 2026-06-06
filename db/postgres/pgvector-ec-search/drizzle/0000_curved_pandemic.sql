CREATE TABLE "products" (
	"id" serial PRIMARY KEY NOT NULL,
	"slug" text NOT NULL,
	"name" text NOT NULL,
	"category" text NOT NULL,
	"description" text NOT NULL,
	"tags" text[] NOT NULL,
	"price_cents" integer NOT NULL,
	"rating" real NOT NULL,
	"popularity_score" integer NOT NULL,
	"stock" integer NOT NULL,
	"embedding" vector(12) NOT NULL,
	"created_at" timestamp with time zone DEFAULT now() NOT NULL,
	"updated_at" timestamp with time zone DEFAULT now() NOT NULL,
	CONSTRAINT "products_slug_unique" UNIQUE("slug")
);
--> statement-breakpoint
CREATE INDEX "products_category_idx" ON "products" USING btree ("category");--> statement-breakpoint
CREATE INDEX "products_embedding_hnsw_idx" ON "products" USING hnsw ("embedding" vector_cosine_ops);
