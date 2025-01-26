import { db } from './db';
import { SalesSummary, Granularity } from './types';

class SalesAnalyzer {
  async createSummaryView(granularity: Granularity): Promise<void> {
    try {
      await db.executeProc('create_sales_summary_mv', [granularity]);
      console.log(`Created sales summary view for ${granularity}`);
    } catch (error) {
      console.error('Error creating summary view:', error);
      throw error;
    }
  }

  async getSummary(granularity: Granularity): Promise<SalesSummary[]> {
    try {
      const results = await db.query<SalesSummary>(
        `SELECT * FROM sales_summary_${granularity} LIMIT 10`
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
        FROM sales_summary_${granularity}
        ORDER BY period DESC
        LIMIT 5
      `);

      console.log(`\nTrend Analysis (${granularity}):`);
      results.forEach(row => {
        console.log(
          `Period: ${row.period.toISOString().split('T')[0]}, ` +
          `Amount: $${row.total_amount}, ` +
          `Growth: ${row.growth_rate}%`
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

main();
