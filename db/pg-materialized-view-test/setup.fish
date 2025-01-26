#!/usr/bin/env fish

# プロジェクト作成とセットアップ
mkdir -p pg-materialized-view-test
cd pg-materialized-view-test
mkdir -p docker/init src

# 必要なファイルの作成
# package.json
echo '{
 "name": "pg-materialized-view-test",
 "version": "1.0.0",
 "scripts": {
   "dev": "ts-node src/index.ts",
   "build": "tsc",
   "start": "node dist/index.js",
   "create-proc": "docker exec -i postgres_test psql -U testuser -d testdb < procedures/create_stats_proc.sql"
 }
}' > package.json

# パッケージインストール
npm install pg @types/pg typescript ts-node dotenv
npm install -D @types/node

echo "node_modules
dist
.env" > .gitignore

# procedures ディレクトリ作成とSQLファイル
mkdir -p procedures
echo "CREATE OR REPLACE PROCEDURE update_daily_stats()
LANGUAGE plpgsql
AS \$\$
DECLARE
    today_stats JSONB;
BEGIN
    SELECT jsonb_build_object(
        'orders', (
            SELECT jsonb_build_object(
                'total_count', COUNT(*),
                'total_amount', SUM(amount),
                'avg_amount', AVG(amount)
            )
            FROM orders
            WHERE DATE(order_date) = CURRENT_DATE
        )
    ) INTO today_stats;

    INSERT INTO daily_stats (stats_date, stats_data)
    VALUES (CURRENT_DATE, today_stats)
    ON CONFLICT (stats_date)
    DO UPDATE SET stats_data = EXCLUDED.stats_data;
END;
\$\$;" > procedures/create_stats_proc.sql

# TypeScript設定
echo '{
 "compilerOptions": {
   "target": "ES2020",
   "module": "commonjs",
   "outDir": "./dist",
   "rootDir": "./src",
   "strict": true,
   "esModuleInterop": true,
   "skipLibCheck": true,
   "forceConsistentCasingInFileNames": true
 }
}' > tsconfig.json

# ディレクトリ作成
mkdir -p docker/init
mkdir -p src

# Docker設定
echo 'version: "3.8"

services:
  db:
    image: postgres:15
    container_name: postgres_test
    environment:
      POSTGRES_USER: testuser
      POSTGRES_PASSWORD: testpass
      POSTGRES_DB: testdb
    ports:
      - "5432:5432"
    volumes:
      - pg_data:/var/lib/postgresql/data
      - ./init:/docker-entrypoint-initdb.d

volumes:
  pg_data:' > docker/docker-compose.yml

# 初期化SQL
echo "CREATE TABLE orders (
    order_id SERIAL PRIMARY KEY,
    order_date DATE NOT NULL,
    customer_id INTEGER NOT NULL,
    amount DECIMAL(10,2) NOT NULL
);

INSERT INTO orders (order_date, customer_id, amount)
SELECT 
    CURRENT_DATE - (random() * 365)::INTEGER,
    (random() * 100)::INTEGER + 1,
    (random() * 1000)::DECIMAL(10,2)
FROM generate_series(1, 1000);

CREATE OR REPLACE PROCEDURE create_sales_summary_mv(
    in_granularity text
)
LANGUAGE plpgsql
AS \$\$
DECLARE
    mv_name text;
BEGIN
    mv_name := format('sales_summary_%s', in_granularity);
    
    EXECUTE format('DROP MATERIALIZED VIEW IF EXISTS %I', mv_name);
    
    EXECUTE format(
        'CREATE MATERIALIZED VIEW %I AS
         SELECT 
             DATE_TRUNC(\$1, order_date) as period,
             COUNT(*) as order_count,
             SUM(amount) as total_amount,
             AVG(amount) as avg_amount
         FROM orders
         GROUP BY DATE_TRUNC(\$1, order_date)
         ORDER BY period DESC
         WITH DATA',
         mv_name
    ) USING in_granularity;
    
    EXECUTE format(
        'CREATE INDEX %I ON %I (period)',
        'idx_' || mv_name,
        mv_name
    );
END;
\$\$;" > docker/init/01_init.sql

# TypeScriptソースファイル
echo "import dotenv from 'dotenv';

dotenv.config();

export const config = {
  db: {
    host: process.env.DB_HOST || 'localhost',
    port: parseInt(process.env.DB_PORT || '5432'),
    database: process.env.DB_NAME || 'testdb',
    user: process.env.DB_USER || 'testuser',
    password: process.env.DB_PASSWORD || 'testpass'
  }
};" > src/config.ts

echo "import { Pool, PoolClient } from 'pg';
import { config } from './config';

class Database {
  private pool: Pool;

  constructor() {
    this.pool = new Pool(config.db);
  }

  async getClient(): Promise<PoolClient> {
    return await this.pool.connect();
  }

  async query<T>(sql: string, params?: any[]): Promise<T[]> {
    const client = await this.getClient();
    try {
      const result = await client.query(sql, params);
      return result.rows;
    } finally {
      client.release();
    }
  }

  async executeProc(procName: string, params?: any[]): Promise<void> {
    const client = await this.getClient();
    try {
      await client.query('CALL ' + procName + '(\$1)', params);
    } finally {
      client.release();
    }
  }
}

export const db = new Database();" > src/db.ts

echo "export interface SalesSummary {
  period: Date;
  order_count: number;
  total_amount: number;
  avg_amount: number;
}

export type Granularity = 'day' | 'week' | 'month' | 'year';" > src/types.ts

echo "import { db } from './db';
import { SalesSummary, Granularity } from './types';

class SalesAnalyzer {
  async createSummaryView(granularity: Granularity): Promise<void> {
    try {
      await db.executeProc('create_sales_summary_mv', [granularity]);
      console.log(`Created sales summary view for \${granularity}`);
    } catch (error) {
      console.error('Error creating summary view:', error);
      throw error;
    }
  }

  async getSummary(granularity: Granularity): Promise<SalesSummary[]> {
    try {
      const results = await db.query<SalesSummary>(
        `SELECT * FROM sales_summary_\${granularity} LIMIT 10`
      );
      return results;
    } catch (error) {
      console.error('Error getting summary:', error);
      throw error;
    }
  }

  async analyzeTrends(granularity: Granularity): Promise<void> {
    try {
      const results = await db.query<any>(`
        SELECT 
          period,
          total_amount,
          lag(total_amount) OVER (ORDER BY period) as prev_amount,
          ROUND(
            ((total_amount - lag(total_amount) OVER (ORDER BY period)) / 
            lag(total_amount) OVER (ORDER BY period) * 100)::numeric,
            2
          ) as growth_rate
        FROM sales_summary_\${granularity}
        ORDER BY period DESC
        LIMIT 5
      `);

      console.log(`\nTrend Analysis (\${granularity}):`);
      results.forEach(row => {
        console.log(
          `Period: \${row.period.toISOString().split('T')[0]}, ` +
          `Amount: \$\${row.total_amount}, ` +
          `Growth: \${row.growth_rate}%`
        );
      });
    } catch (error) {
      console.error('Error analyzing trends:', error);
      throw error;
    }
  }
}

async function main() {
  const analyzer = new SalesAnalyzer();

  try {
    await analyzer.createSummaryView('day');
    const dailySummary = await analyzer.getSummary('day');
    console.log('\nDaily Summary:', dailySummary);
    await analyzer.analyzeTrends('day');

    await analyzer.createSummaryView('month');
    const monthlySummary = await analyzer.getSummary('month');
    console.log('\nMonthly Summary:', monthlySummary);
    await analyzer.analyzeTrends('month');

  } catch (error) {
    console.error('Error in main:', error);
  }
}

main();" > src/index.ts

# 環境変数ファイル
echo "DB_HOST=localhost
DB_PORT=5432
DB_NAME=testdb
DB_USER=testuser
DB_PASSWORD=testpass" > .env

# Docker起動
cd docker
docker-compose up -d
cd ..

# アプリケーション実行
npm run dev