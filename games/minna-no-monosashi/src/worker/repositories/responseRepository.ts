import type { PublicBlockStatus } from "../../shared/contracts";

export type QuestionContext = {
  blockId: string;
  status: "draft" | PublicBlockStatus;
  questionId: string;
  optionId: string | null;
};

export type AggregatedOption = {
  id: string;
  label: string;
  count: number;
};

type ExistingResponseRow = { option_id: string };
type AggregateRow = { id: string; label: string; response_count: number };

export class ResponseRepository {
  constructor(private readonly db: D1Database) {}

  findQuestionContext(
    slug: string,
    questionId: string,
    optionId?: string,
  ): Promise<QuestionContext | null> {
    return this.db
      .prepare(
        `SELECT
           b.id AS blockId,
           b.status,
           q.id AS questionId,
           o.id AS optionId
         FROM blocks b
         JOIN questions q ON q.block_id = b.id AND q.id = ?
         LEFT JOIN options o ON o.question_id = q.id AND o.id = ?
         WHERE b.slug = ?`,
      )
      .bind(questionId, optionId ?? "", slug)
      .first<QuestionContext>();
  }

  async findExisting(
    questionId: string,
    voterHash: string,
  ): Promise<string | null> {
    const row = await this.db
      .prepare(
        "SELECT option_id FROM responses WHERE question_id = ? AND voter_key_hash = ?",
      )
      .bind(questionId, voterHash)
      .first<ExistingResponseRow>();
    return row?.option_id ?? null;
  }

  async insert(input: {
    blockId: string;
    questionId: string;
    optionId: string;
    voterHash: string;
  }): Promise<void> {
    await this.db
      .prepare(
        `INSERT INTO responses (
          id, block_id, question_id, option_id, voter_key_hash, created_at
        ) VALUES (?, ?, ?, ?, ?, ?)`,
      )
      .bind(
        crypto.randomUUID(),
        input.blockId,
        input.questionId,
        input.optionId,
        input.voterHash,
        new Date().toISOString(),
      )
      .run();
  }

  async aggregate(questionId: string): Promise<AggregatedOption[]> {
    const result = await this.db
      .prepare(
        `SELECT o.id, o.label, COUNT(r.id) AS response_count
         FROM options o
         LEFT JOIN responses r
           ON r.option_id = o.id AND r.question_id = o.question_id
         WHERE o.question_id = ?
         GROUP BY o.id
         ORDER BY o.position ASC`,
      )
      .bind(questionId)
      .all<AggregateRow>();
    return result.results.map((row) => ({
      id: row.id,
      label: row.label,
      count: Number(row.response_count),
    }));
  }

  async progress(blockId: string, voterHash: string): Promise<string[]> {
    const result = await this.db
      .prepare(
        `SELECT r.question_id
         FROM responses r
         JOIN questions q ON q.id = r.question_id
         WHERE r.block_id = ? AND r.voter_key_hash = ?
         ORDER BY q.position ASC`,
      )
      .bind(blockId, voterHash)
      .all<{ question_id: string }>();
    return result.results.map((row) => row.question_id);
  }

  async listQuestionIds(blockId: string): Promise<string[]> {
    const result = await this.db
      .prepare(
        "SELECT id FROM questions WHERE block_id = ? ORDER BY position ASC",
      )
      .bind(blockId)
      .all<{ id: string }>();
    return result.results.map((row) => row.id);
  }

  async findPublicBlock(
    slug: string,
  ): Promise<{ id: string; status: PublicBlockStatus } | null> {
    return this.db
      .prepare(
        "SELECT id, status FROM blocks WHERE slug = ? AND status IN ('published', 'closed')",
      )
      .bind(slug)
      .first<{ id: string; status: PublicBlockStatus }>();
  }
}
