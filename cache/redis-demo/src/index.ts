import express from "express";
import pg from "pg";
import { createClient } from "redis";

const app = express();
const port = 3000;

// PostgreSQL接続
const pool = new pg.Pool({
  connectionString: process.env.DATABASE_URL,
});

// Redis接続
const redis = createClient({
  url: process.env.REDIS_URL,
});

redis.on("error", (err) => console.error("Redis Client Error", err));

await redis.connect();

// キャッシュなし - 毎回DBにアクセス（非常に重いクエリ: 複数JOIN + サブクエリ + 集計）
app.get("/api/products/no-cache", async (req, res) => {
  const start = performance.now();

  // 超重いクエリ: 3つのテーブルをJOIN + サブクエリで集計 + 複雑なソート
  const { rows } = await pool.query(`
    SELECT
      p.*,
      LENGTH(p.description) as description_length,
      p.specifications->>'warranty_years' as warranty,
      p.price * 1.1 as price_with_tax,
      COALESCE(COUNT(DISTINCT r.id), 0) as review_count_calc,
      COALESCE(AVG(r.rating), 0)::DECIMAL(3, 2) as avg_review_rating,
      COALESCE(COUNT(DISTINCT o.id), 0) as order_count,
      COALESCE(SUM(o.quantity), 0) as total_sold,
      COALESCE(SUM(o.total_price), 0)::DECIMAL(10, 2) as total_revenue,
      (
        SELECT COUNT(*)
        FROM inventory_logs il
        WHERE il.product_id = p.id AND il.reason = 'sale'
      ) as sale_log_count,
      (
        SELECT STRING_AGG(DISTINCT user_name, ', ')
        FROM reviews r2
        WHERE r2.product_id = p.id AND r2.rating >= 4
        LIMIT 5
      ) as top_reviewers
    FROM products p
    LEFT JOIN reviews r ON p.id = r.product_id
    LEFT JOIN orders o ON p.id = o.product_id
    WHERE p.category = $1
    GROUP BY p.id
    HAVING COUNT(DISTINCT r.id) > 0
    ORDER BY
      AVG(r.rating) DESC,
      COUNT(DISTINCT o.id) DESC,
      p.price DESC,
      p.created_at DESC
    LIMIT 100
  `, [req.query.category || "Electronics"]);

  const duration = performance.now() - start;

  res.json({
    source: "database",
    duration: `${duration.toFixed(2)}ms`,
    count: rows.length,
    data: rows,
  });
});

// キャッシュあり - Redisにあればそれを返す（非常に重いクエリ）
app.get("/api/products/with-cache", async (req, res) => {
  const start = performance.now();
  const category = req.query.category || "Electronics";
  const cacheKey = `products:${category}`;

  // Redisから取得を試みる
  const cached = await redis.get(cacheKey);

  if (cached) {
    const duration = performance.now() - start;
    return res.json({
      source: "redis",
      duration: `${duration.toFixed(2)}ms`,
      count: JSON.parse(cached).length,
      data: JSON.parse(cached),
    });
  }

  // キャッシュがなければDBから取得（キャッシュなしと同じ超重いクエリ）
  const { rows } = await pool.query(`
    SELECT
      p.*,
      LENGTH(p.description) as description_length,
      p.specifications->>'warranty_years' as warranty,
      p.price * 1.1 as price_with_tax,
      COALESCE(COUNT(DISTINCT r.id), 0) as review_count_calc,
      COALESCE(AVG(r.rating), 0)::DECIMAL(3, 2) as avg_review_rating,
      COALESCE(COUNT(DISTINCT o.id), 0) as order_count,
      COALESCE(SUM(o.quantity), 0) as total_sold,
      COALESCE(SUM(o.total_price), 0)::DECIMAL(10, 2) as total_revenue,
      (
        SELECT COUNT(*)
        FROM inventory_logs il
        WHERE il.product_id = p.id AND il.reason = 'sale'
      ) as sale_log_count,
      (
        SELECT STRING_AGG(DISTINCT user_name, ', ')
        FROM reviews r2
        WHERE r2.product_id = p.id AND r2.rating >= 4
        LIMIT 5
      ) as top_reviewers
    FROM products p
    LEFT JOIN reviews r ON p.id = r.product_id
    LEFT JOIN orders o ON p.id = o.product_id
    WHERE p.category = $1
    GROUP BY p.id
    HAVING COUNT(DISTINCT r.id) > 0
    ORDER BY
      AVG(r.rating) DESC,
      COUNT(DISTINCT o.id) DESC,
      p.price DESC,
      p.created_at DESC
    LIMIT 100
  `, [category]);

  // Redisにキャッシュ（60秒間有効）
  await redis.setEx(cacheKey, 60, JSON.stringify(rows));

  const duration = performance.now() - start;

  res.json({
    source: "database (cached for next request)",
    duration: `${duration.toFixed(2)}ms`,
    count: rows.length,
    data: rows,
  });
});

// キャッシュクリア
app.post("/api/cache/clear", async (req, res) => {
  await redis.flushAll();
  res.json({ message: "Cache cleared" });
});

// ベンチマーク - 連続リクエストで比較
app.get("/api/benchmark", async (req, res) => {
  const iterations = 10;
  const category = "Electronics";

  // キャッシュクリア
  await redis.flushAll();

  // キャッシュなしで10回（超重いクエリ）
  const noCacheTimes: number[] = [];
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await pool.query(`
      SELECT
        p.*,
        LENGTH(p.description) as description_length,
        p.specifications->>'warranty_years' as warranty,
        p.price * 1.1 as price_with_tax,
        COALESCE(COUNT(DISTINCT r.id), 0) as review_count_calc,
        COALESCE(AVG(r.rating), 0)::DECIMAL(3, 2) as avg_review_rating,
        COALESCE(COUNT(DISTINCT o.id), 0) as order_count,
        COALESCE(SUM(o.quantity), 0) as total_sold,
        COALESCE(SUM(o.total_price), 0)::DECIMAL(10, 2) as total_revenue,
        (
          SELECT COUNT(*)
          FROM inventory_logs il
          WHERE il.product_id = p.id AND il.reason = 'sale'
        ) as sale_log_count,
        (
          SELECT STRING_AGG(DISTINCT user_name, ', ')
          FROM reviews r2
          WHERE r2.product_id = p.id AND r2.rating >= 4
          LIMIT 5
        ) as top_reviewers
      FROM products p
      LEFT JOIN reviews r ON p.id = r.product_id
      LEFT JOIN orders o ON p.id = o.product_id
      WHERE p.category = $1
      GROUP BY p.id
      HAVING COUNT(DISTINCT r.id) > 0
      ORDER BY
        AVG(r.rating) DESC,
        COUNT(DISTINCT o.id) DESC,
        p.price DESC,
        p.created_at DESC
      LIMIT 100
    `, [category]);
    noCacheTimes.push(performance.now() - start);
  }

  // キャッシュありで10回（最初の1回はDBアクセス）
  await redis.flushAll();
  const withCacheTimes: number[] = [];
  const cacheKey = `products:${category}`;

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    const cached = await redis.get(cacheKey);

    if (cached) {
      JSON.parse(cached);
    } else {
      const { rows } = await pool.query(`
        SELECT
          p.*,
          LENGTH(p.description) as description_length,
          p.specifications->>'warranty_years' as warranty,
          p.price * 1.1 as price_with_tax,
          COALESCE(COUNT(DISTINCT r.id), 0) as review_count_calc,
          COALESCE(AVG(r.rating), 0)::DECIMAL(3, 2) as avg_review_rating,
          COALESCE(COUNT(DISTINCT o.id), 0) as order_count,
          COALESCE(SUM(o.quantity), 0) as total_sold,
          COALESCE(SUM(o.total_price), 0)::DECIMAL(10, 2) as total_revenue,
          (
            SELECT COUNT(*)
            FROM inventory_logs il
            WHERE il.product_id = p.id AND il.reason = 'sale'
          ) as sale_log_count,
          (
            SELECT STRING_AGG(DISTINCT user_name, ', ')
            FROM reviews r2
            WHERE r2.product_id = p.id AND r2.rating >= 4
            LIMIT 5
          ) as top_reviewers
        FROM products p
        LEFT JOIN reviews r ON p.id = r.product_id
        LEFT JOIN orders o ON p.id = o.product_id
        WHERE p.category = $1
        GROUP BY p.id
        HAVING COUNT(DISTINCT r.id) > 0
        ORDER BY
          AVG(r.rating) DESC,
          COUNT(DISTINCT o.id) DESC,
          p.price DESC,
          p.created_at DESC
        LIMIT 100
      `, [category]);
      await redis.setEx(cacheKey, 60, JSON.stringify(rows));
    }

    withCacheTimes.push(performance.now() - start);
  }

  const avg = (arr: number[]) => arr.reduce((a, b) => a + b, 0) / arr.length;

  res.json({
    iterations,
    noCache: {
      times: noCacheTimes.map((t) => `${t.toFixed(2)}ms`),
      average: `${avg(noCacheTimes).toFixed(2)}ms`,
    },
    withCache: {
      times: withCacheTimes.map((t) => `${t.toFixed(2)}ms`),
      average: `${avg(withCacheTimes).toFixed(2)}ms`,
      note: "First request hits DB, rest from Redis",
    },
    speedup: `${(avg(noCacheTimes) / avg(withCacheTimes)).toFixed(1)}x faster with cache`,
  });
});

// シンプルなUI
app.get("/", (req, res) => {
  res.send(`
    <!DOCTYPE html>
    <html>
    <head>
      <title>Redis Cache Demo</title>
      <style>
        body { font-family: system-ui; max-width: 800px; margin: 40px auto; padding: 0 20px; }
        button { padding: 10px 20px; margin: 5px; cursor: pointer; font-size: 16px; }
        pre { background: #f4f4f4; padding: 15px; overflow-x: auto; border-radius: 4px; }
        .result { margin-top: 20px; }
        h1 { color: #333; }
        .buttons { margin: 20px 0; }
      </style>
    </head>
    <body>
      <h1>🚀 Redis Cache Demo</h1>
      <p>PostgreSQLからの商品データ取得を、Redisキャッシュあり/なしで比較します。</p>
      
      <div class="buttons">
        <button onclick="fetchData('/api/products/no-cache')">DBから直接取得</button>
        <button onclick="fetchData('/api/products/with-cache')">Redisキャッシュ経由</button>
        <button onclick="fetchData('/api/benchmark')">ベンチマーク (10回比較)</button>
        <button onclick="clearCache()">キャッシュクリア</button>
      </div>
      
      <div class="result">
        <pre id="result">結果がここに表示されます</pre>
      </div>

      <script>
        const fetchData = async (url) => {
          document.getElementById('result').textContent = 'Loading...';
          const res = await fetch(url);
          const data = await res.json();
          // dataは大きいので省略表示
          const display = { ...data, data: data.data ? '[' + data.count + ' items...]' : undefined };
          document.getElementById('result').textContent = JSON.stringify(display, null, 2);
        };
        
        const clearCache = async () => {
          await fetch('/api/cache/clear', { method: 'POST' });
          document.getElementById('result').textContent = 'Cache cleared!';
        };
      </script>
    </body>
    </html>
  `);
});

app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});
