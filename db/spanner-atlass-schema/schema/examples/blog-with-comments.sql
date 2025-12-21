-- Blog with Comments Schema Example
-- Demonstrates nested interleaved tables (3 levels)

-- Authors table
CREATE TABLE authors (
  author_id INT64 NOT NULL,
  username STRING(50) NOT NULL,
  email STRING(255) NOT NULL,
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (author_id);

-- Blog posts table (interleaved in authors)
CREATE TABLE blog_posts (
  author_id INT64 NOT NULL,
  post_id INT64 NOT NULL,
  title STRING(255) NOT NULL,
  content STRING(MAX),
  published BOOL NOT NULL DEFAULT (false),
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true),
  updated_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (author_id, post_id),
  INTERLEAVE IN PARENT authors ON DELETE CASCADE;

-- Comments table (interleaved in blog_posts)
CREATE TABLE comments (
  author_id INT64 NOT NULL,
  post_id INT64 NOT NULL,
  comment_id INT64 NOT NULL,
  commenter_name STRING(255) NOT NULL,
  commenter_email STRING(255),
  content STRING(1024) NOT NULL,
  created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)
) PRIMARY KEY (author_id, post_id, comment_id),
  INTERLEAVE IN PARENT blog_posts ON DELETE CASCADE;

-- Tags table
CREATE TABLE tags (
  tag_id INT64 NOT NULL,
  name STRING(50) NOT NULL
) PRIMARY KEY (tag_id);

-- Post tags junction table
CREATE TABLE post_tags (
  author_id INT64 NOT NULL,
  post_id INT64 NOT NULL,
  tag_id INT64 NOT NULL
) PRIMARY KEY (author_id, post_id, tag_id);

-- Index for finding posts by tag
CREATE INDEX posts_by_tag ON post_tags (tag_id);

-- Index for recent posts
CREATE INDEX recent_posts ON blog_posts (created_at DESC);
