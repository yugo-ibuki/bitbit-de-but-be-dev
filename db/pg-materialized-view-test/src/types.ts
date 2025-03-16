export interface SalesSummary {
  period: Date;
  order_count: number;
  total_amount: number;
  avg_amount: number;
}

export type Granularity = 'day' | 'week' | 'month' | 'year';
