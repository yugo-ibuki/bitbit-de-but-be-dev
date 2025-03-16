CREATE OR REPLACE PROCEDURE update_daily_stats()
LANGUAGE plpgsql
AS $$
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
$$;
