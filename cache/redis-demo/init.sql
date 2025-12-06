-- 商品テーブル
CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price DECIMAL(10, 2),
    category VARCHAR(100),
    stock INTEGER,
    rating DECIMAL(3, 2),
    review_count INTEGER,
    manufacturer VARCHAR(200),
    tags TEXT,
    specifications JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 膨大なダミーデータを挿入（500,000件 - 5カテゴリで各100,000件）
INSERT INTO products (name, description, price, category, stock, rating, review_count, manufacturer, tags, specifications)
SELECT
    'Product ' || i || ' - ' ||
    CASE (i % 5)
        WHEN 0 THEN 'High-Performance Gaming Laptop'
        WHEN 1 THEN 'Premium Cotton T-Shirt Collection'
        WHEN 2 THEN 'Best Seller Novel Series'
        WHEN 3 THEN 'Modern Furniture Set'
        WHEN 4 THEN 'Professional Sports Equipment'
    END,
    'This is a highly detailed description for product ' || i || '. ' ||
    'It contains extensive information about the product features, technical specifications, ' ||
    'warranty details, customer reviews, usage instructions, safety guidelines, ' ||
    'compatibility information, material composition, dimensions, weight, ' ||
    'color options, available sizes, shipping information, and other relevant details ' ||
    'that make the database query processing more intensive and realistic. ' ||
    'Additional content to increase row size: Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' ||
    'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ' ||
    'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.',
    (random() * 5000 + 10)::DECIMAL(10, 2),
    CASE (i % 5)
        WHEN 0 THEN 'Electronics'
        WHEN 1 THEN 'Clothing'
        WHEN 2 THEN 'Books'
        WHEN 3 THEN 'Home'
        WHEN 4 THEN 'Sports'
    END,
    (random() * 1000)::INTEGER,
    (random() * 5)::DECIMAL(3, 2),
    (random() * 10000)::INTEGER,
    'Manufacturer-' || (i % 100),
    'tag1, tag2, tag3, tag4, tag5, bestseller, premium, recommended',
    jsonb_build_object(
        'weight', (random() * 50 + 0.5)::DECIMAL(10, 2),
        'dimensions', jsonb_build_object('width', random() * 100, 'height', random() * 100, 'depth', random() * 100),
        'color_options', jsonb_build_array('black', 'white', 'blue', 'red', 'green'),
        'warranty_years', (random() * 5 + 1)::INTEGER,
        'origin_country', CASE (i % 10) WHEN 0 THEN 'USA' WHEN 1 THEN 'Japan' WHEN 2 THEN 'Germany' ELSE 'China' END
    )
FROM generate_series(1, 500000) AS i;

-- レビューテーブル（各商品に複数レビュー）
CREATE TABLE reviews (
    id SERIAL PRIMARY KEY,
    product_id INTEGER REFERENCES products(id),
    user_name VARCHAR(100),
    rating INTEGER,
    comment TEXT,
    helpful_count INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 注文テーブル
CREATE TABLE orders (
    id SERIAL PRIMARY KEY,
    product_id INTEGER REFERENCES products(id),
    quantity INTEGER,
    total_price DECIMAL(10, 2),
    customer_name VARCHAR(100),
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 在庫履歴テーブル
CREATE TABLE inventory_logs (
    id SERIAL PRIMARY KEY,
    product_id INTEGER REFERENCES products(id),
    change_amount INTEGER,
    reason VARCHAR(50),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- レビューデータ（2,000,000件 - 各商品に平均4件）
INSERT INTO reviews (product_id, user_name, rating, comment, helpful_count)
SELECT
    (random() * 499999 + 1)::INTEGER,
    'User-' || i,
    (random() * 5)::INTEGER,
    'This is a review comment ' || i || '. ' ||
    'The product quality is good. I am satisfied with my purchase. ' ||
    'Delivery was fast and packaging was excellent. Would recommend to others.',
    (random() * 100)::INTEGER
FROM generate_series(1, 2000000) AS i;

-- 注文データ（1,000,000件）
INSERT INTO orders (product_id, quantity, total_price, customer_name)
SELECT
    (random() * 499999 + 1)::INTEGER,
    (random() * 10 + 1)::INTEGER,
    (random() * 10000 + 100)::DECIMAL(10, 2),
    'Customer-' || i
FROM generate_series(1, 1000000) AS i;

-- 在庫履歴データ（1,500,000件）
INSERT INTO inventory_logs (product_id, change_amount, reason)
SELECT
    (random() * 499999 + 1)::INTEGER,
    (random() * 200 - 100)::INTEGER,
    CASE (i % 4)
        WHEN 0 THEN 'restock'
        WHEN 1 THEN 'sale'
        WHEN 2 THEN 'return'
        ELSE 'adjustment'
    END
FROM generate_series(1, 1500000) AS i;

-- インデックス作成
CREATE INDEX idx_products_category ON products(category);
CREATE INDEX idx_products_price ON products(price);
CREATE INDEX idx_products_rating ON products(rating);
CREATE INDEX idx_reviews_product_id ON reviews(product_id);
CREATE INDEX idx_reviews_rating ON reviews(rating);
CREATE INDEX idx_orders_product_id ON orders(product_id);
CREATE INDEX idx_inventory_logs_product_id ON inventory_logs(product_id);

-- マテリアライズドビューで集計を事前計算（比較用）
CREATE MATERIALIZED VIEW product_stats AS
SELECT
    p.id,
    COUNT(DISTINCT r.id) as total_reviews,
    AVG(r.rating)::DECIMAL(3, 2) as avg_review_rating,
    COUNT(DISTINCT o.id) as total_orders,
    SUM(o.quantity) as total_quantity_sold
FROM products p
LEFT JOIN reviews r ON p.id = r.product_id
LEFT JOIN orders o ON p.id = o.product_id
GROUP BY p.id;

CREATE INDEX idx_product_stats_id ON product_stats(id);
