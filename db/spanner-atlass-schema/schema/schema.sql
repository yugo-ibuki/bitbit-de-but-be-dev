-- Google Cloud Spanner Schema Definition
-- This is the main schema file managed by Atlas

-- Users table
CREATE TABLE users (
  id INT64 NOT NULL,
  email STRING(255),
  display_name STRING(255),
  bio STRING(1024),
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  updated_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (id);

-- Posts table with foreign key to users
CREATE TABLE posts (
  id INT64 NOT NULL,
  title STRING(255) NOT NULL,
  body STRING(MAX),
  author_id INT64 NOT NULL,
  published BOOL NOT NULL DEFAULT (false),
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  updated_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  CONSTRAINT author_fk FOREIGN KEY (author_id) REFERENCES users (id)
) PRIMARY KEY (id);

-- Index on posts by author
CREATE INDEX posts_by_author ON posts (author_id);

-- Index on posts by creation date
CREATE INDEX posts_by_created_at ON posts (created_at DESC);
