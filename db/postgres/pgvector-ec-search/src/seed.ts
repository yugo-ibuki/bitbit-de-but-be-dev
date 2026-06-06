import { db, queryClient } from "./db.js";
import { buildSeedProducts } from "./seed-data.js";
import { products } from "./schema.js";

const seedProducts = buildSeedProducts();

await db.delete(products);
await db.insert(products).values(seedProducts);

console.log(`Seeded ${seedProducts.length} products`);
await queryClient.end();
