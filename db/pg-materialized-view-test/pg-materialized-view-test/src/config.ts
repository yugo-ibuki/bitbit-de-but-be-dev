import dotenv from 'dotenv';

dotenv.config();

export const config = {
  db: {
    host: process.env.DB_HOST || 'localhost',
    port: parseInt(process.env.DB_PORT || '5432'),
    database: process.env.DB_NAME || 'testdb',
    user: process.env.DB_USER || 'testuser',
    password: process.env.DB_PASSWORD || 'testpass'
  }
};
