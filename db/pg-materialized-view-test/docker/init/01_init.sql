CREATE TABLE orders (
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
AS $$
DECLARE
    mv_name text;
BEGIN
    mv_name := format('sales_summary_%s', in_granularity);
    
    EXECUTE format('DROP MATERIALIZED VIEW IF EXISTS %I', mv_name);
    
    EXECUTE format(
        'CREATE MATERIALIZED VIEW %I AS
         SELECT 
             DATE_TRUNC($1, order_date) as period,
             COUNT(*) as order_count,
             SUM(amount) as total_amount,
             AVG(amount) as avg_amount
         FROM orders
         GROUP BY DATE_TRUNC($1, order_date)
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
$$;
