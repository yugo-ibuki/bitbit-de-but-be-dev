PRAGMA foreign_keys = ON;

CREATE TABLE blocks (
  id TEXT PRIMARY KEY,
  slug TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '',
  status TEXT NOT NULL CHECK(status IN ('draft', 'published', 'closed')),
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  published_at TEXT,
  closed_at TEXT
);

CREATE TABLE questions (
  id TEXT PRIMARY KEY,
  block_id TEXT NOT NULL REFERENCES blocks(id) ON DELETE CASCADE,
  prompt TEXT NOT NULL,
  position INTEGER NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE(block_id, position)
);

CREATE TABLE options (
  id TEXT PRIMARY KEY,
  question_id TEXT NOT NULL REFERENCES questions(id) ON DELETE CASCADE,
  label TEXT NOT NULL,
  position INTEGER NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE(question_id, position)
);

CREATE TABLE responses (
  id TEXT PRIMARY KEY,
  block_id TEXT NOT NULL REFERENCES blocks(id),
  question_id TEXT NOT NULL REFERENCES questions(id),
  option_id TEXT NOT NULL REFERENCES options(id),
  voter_key_hash TEXT NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE(question_id, voter_key_hash)
);

CREATE INDEX idx_responses_question_option
  ON responses(question_id, option_id);
CREATE INDEX idx_responses_block_voter
  ON responses(block_id, voter_key_hash);
