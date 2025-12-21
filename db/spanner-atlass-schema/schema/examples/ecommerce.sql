-- E-commerce Schema Example for Google Cloud Spanner
-- Demonstrates interleaved tables and more complex relationships

-- Customers table
CREATE TABLE customers (
  customer_id INT64 NOT NULL,
  email STRING(255) NOT NULL,
  name STRING(255) NOT NULL,
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (customer_id);

-- Orders table (parent table for order_items)
CREATE TABLE orders (
  customer_id INT64 NOT NULL,
  order_id INT64 NOT NULL,
  status STRING(50) NOT NULL,
  total_amount NUMERIC NOT NULL,
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  CONSTRAINT customer_fk FOREIGN KEY (customer_id) REFERENCES customers (customer_id)
) PRIMARY KEY (customer_id, order_id),
  INTERLEAVE IN PARENT customers ON DELETE CASCADE;

-- Order items table (child of orders)
CREATE TABLE order_items (
  customer_id INT64 NOT NULL,
  order_id INT64 NOT NULL,
  item_id INT64 NOT NULL,
  product_id INT64 NOT NULL,
  quantity INT64 NOT NULL,
  price NUMERIC NOT NULL
) PRIMARY KEY (customer_id, order_id, item_id),
  INTERLEAVE IN PARENT orders ON DELETE CASCADE;

-- Products table
CREATE TABLE products (
  product_id INT64 NOT NULL,
  name STRING(255) NOT NULL,
  description STRING(MAX),
  price NUMERIC NOT NULL,
  stock_quantity INT64 NOT NULL,
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (product_id);

-- Global index for orders by status
CREATE INDEX orders_by_status ON orders (status);

-- Index for products by price
CREATE INDEX products_by_price ON products (price);
