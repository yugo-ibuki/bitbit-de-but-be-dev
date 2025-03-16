import { Pool, PoolClient } from 'pg';
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
      await client.query('CALL ' + procName + '($1)', params);
    } finally {
      client.release();
    }
  }
}

export const db = new Database();
