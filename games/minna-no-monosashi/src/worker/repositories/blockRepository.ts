import type {
  AdminBlock,
  AdminBlockSummary,
  PublicBlockDetail,
  PublicBlockStatus,
  PublicBlockSummary,
  PublicOption,
  PublicQuestion,
} from "../../shared/contracts";
import type { DraftBlockInput } from "../../domain/types";

type BlockSummaryRow = {
  slug: string;
  title: string;
  description: string;
  status: PublicBlockStatus;
  question_count: number;
  participant_count: number;
};

type QuestionRow = {
  id: string;
  prompt: string;
  position: number;
};

type OptionRow = {
  id: string;
  question_id: string;
  label: string;
  position: number;
};

type AdminBlockRow = {
  id: string;
  slug: string;
  title: string;
  description: string;
  status: "draft" | PublicBlockStatus;
  created_at: string;
  updated_at: string;
  published_at: string | null;
  closed_at: string | null;
};

type AdminSummaryRow = AdminBlockRow & {
  question_count: number;
  participant_count: number;
};

const PUBLIC_SUMMARY_SELECT = `
  SELECT
    b.slug,
    b.title,
    b.description,
    b.status,
    COUNT(DISTINCT q.id) AS question_count,
    COUNT(DISTINCT r.voter_key_hash) AS participant_count
  FROM blocks b
  LEFT JOIN questions q ON q.block_id = b.id
  LEFT JOIN responses r ON r.block_id = b.id
`;

function toSummary(row: BlockSummaryRow): PublicBlockSummary {
  return {
    slug: row.slug,
    title: row.title,
    description: row.description,
    status: row.status,
    questionCount: Number(row.question_count),
    participantCount: Number(row.participant_count),
  };
}

export class BlockRepository {
  constructor(private readonly db: D1Database) {}

  async listPublic(): Promise<PublicBlockSummary[]> {
    const result = await this.db
      .prepare(
        `${PUBLIC_SUMMARY_SELECT}
         WHERE b.status IN ('published', 'closed')
         GROUP BY b.id
         ORDER BY CASE b.status WHEN 'published' THEN 0 ELSE 1 END,
                  COALESCE(b.published_at, b.created_at) DESC,
                  b.created_at DESC`,
      )
      .all<BlockSummaryRow>();

    return result.results.map(toSummary);
  }

  async findPublicBySlug(slug: string): Promise<PublicBlockDetail | null> {
    const summaryRow = await this.db
      .prepare(
        `${PUBLIC_SUMMARY_SELECT}
         WHERE b.slug = ? AND b.status IN ('published', 'closed')
         GROUP BY b.id`,
      )
      .bind(slug)
      .first<BlockSummaryRow>();

    if (!summaryRow) return null;

    const [questionResult, optionResult] = await Promise.all([
      this.db
        .prepare(
          `SELECT q.id, q.prompt, q.position
           FROM questions q
           JOIN blocks b ON b.id = q.block_id
           WHERE b.slug = ?
           ORDER BY q.position ASC`,
        )
        .bind(slug)
        .all<QuestionRow>(),
      this.db
        .prepare(
          `SELECT o.id, o.question_id, o.label, o.position
           FROM options o
           JOIN questions q ON q.id = o.question_id
           JOIN blocks b ON b.id = q.block_id
           WHERE b.slug = ?
           ORDER BY q.position ASC, o.position ASC`,
        )
        .bind(slug)
        .all<OptionRow>(),
    ]);

    const optionsByQuestion = new Map<string, PublicOption[]>();
    for (const option of optionResult.results) {
      const options = optionsByQuestion.get(option.question_id) ?? [];
      options.push({
        id: option.id,
        label: option.label,
        position: Number(option.position),
      });
      optionsByQuestion.set(option.question_id, options);
    }

    const questions: PublicQuestion[] = questionResult.results.map(
      (question) => ({
        id: question.id,
        prompt: question.prompt,
        position: Number(question.position),
        options: optionsByQuestion.get(question.id) ?? [],
      }),
    );

    return { ...toSummary(summaryRow), questions };
  }

  async listAdmin(): Promise<AdminBlockSummary[]> {
    const result = await this.db
      .prepare(
        `SELECT b.*,
           COUNT(DISTINCT q.id) AS question_count,
           COUNT(DISTINCT r.voter_key_hash) AS participant_count
         FROM blocks b
         LEFT JOIN questions q ON q.block_id = b.id
         LEFT JOIN responses r ON r.block_id = b.id
         GROUP BY b.id
         ORDER BY b.updated_at DESC, b.created_at DESC`,
      )
      .all<AdminSummaryRow>();
    return result.results.map((row) => ({
      ...this.toAdminBase(row),
      questionCount: Number(row.question_count),
      participantCount: Number(row.participant_count),
    }));
  }

  async findAdminById(id: string): Promise<AdminBlock | null> {
    const block = await this.db
      .prepare("SELECT * FROM blocks WHERE id = ?")
      .bind(id)
      .first<AdminBlockRow>();
    if (!block) return null;

    const [questionResult, optionResult] = await Promise.all([
      this.db
        .prepare(
          "SELECT id, prompt, position FROM questions WHERE block_id = ? ORDER BY position ASC",
        )
        .bind(id)
        .all<QuestionRow>(),
      this.db
        .prepare(
          `SELECT o.id, o.question_id, o.label, o.position
           FROM options o
           JOIN questions q ON q.id = o.question_id
           WHERE q.block_id = ?
           ORDER BY q.position ASC, o.position ASC`,
        )
        .bind(id)
        .all<OptionRow>(),
    ]);
    const optionsByQuestion = new Map<string, PublicOption[]>();
    for (const option of optionResult.results) {
      const options = optionsByQuestion.get(option.question_id) ?? [];
      options.push({
        id: option.id,
        label: option.label,
        position: Number(option.position),
      });
      optionsByQuestion.set(option.question_id, options);
    }

    return {
      ...this.toAdminBase(block),
      questions: questionResult.results.map((question) => ({
        id: question.id,
        prompt: question.prompt,
        position: Number(question.position),
        options: optionsByQuestion.get(question.id) ?? [],
      })),
    };
  }

  async createDraft(input: DraftBlockInput): Promise<AdminBlock> {
    const id = crypto.randomUUID();
    const now = new Date().toISOString();
    const statements = [
      this.db
        .prepare(
          `INSERT INTO blocks (
             id, slug, title, description, status, created_at, updated_at
           ) VALUES (?, ?, ?, ?, 'draft', ?, ?)`,
        )
        .bind(id, input.slug, input.title, input.description, now, now),
      ...this.contentInsertStatements(id, input, now),
    ];
    await this.db.batch(statements);
    const created = await this.findAdminById(id);
    if (!created) throw new Error("Created block could not be read");
    return created;
  }

  async updateDraft(id: string, input: DraftBlockInput): Promise<AdminBlock | null> {
    const existing = await this.findAdminById(id);
    if (!existing) return null;
    if (existing.status !== "draft") return existing;

    const now = new Date().toISOString();
    await this.db.batch([
      this.db
        .prepare(
          "UPDATE blocks SET slug = ?, title = ?, description = ?, updated_at = ? WHERE id = ? AND status = 'draft'",
        )
        .bind(input.slug, input.title, input.description, now, id),
      this.db.prepare("DELETE FROM questions WHERE block_id = ?").bind(id),
      ...this.contentInsertStatements(id, input, now),
    ]);
    return this.findAdminById(id);
  }

  async deleteDraft(id: string): Promise<"deleted" | "not-found" | "locked"> {
    const block = await this.findAdminById(id);
    if (!block) return "not-found";
    if (block.status !== "draft") return "locked";
    await this.db
      .prepare("DELETE FROM blocks WHERE id = ? AND status = 'draft'")
      .bind(id)
      .run();
    return "deleted";
  }

  async transition(
    id: string,
    from: "draft" | "published",
    to: "published" | "closed",
  ): Promise<AdminBlock | null> {
    const now = new Date().toISOString();
    const timestampColumn = to === "published" ? "published_at" : "closed_at";
    await this.db
      .prepare(
        `UPDATE blocks
         SET status = ?, ${timestampColumn} = ?, updated_at = ?
         WHERE id = ? AND status = ?`,
      )
      .bind(to, now, now, id, from)
      .run();
    return this.findAdminById(id);
  }

  async slugExists(slug: string): Promise<boolean> {
    const row = await this.db
      .prepare("SELECT 1 AS found FROM blocks WHERE slug = ?")
      .bind(slug)
      .first<{ found: number }>();
    return Boolean(row);
  }

  private contentInsertStatements(
    blockId: string,
    input: DraftBlockInput,
    now: string,
  ): D1PreparedStatement[] {
    const statements: D1PreparedStatement[] = [];
    input.questions.forEach((question, questionIndex) => {
      const questionId = crypto.randomUUID();
      statements.push(
        this.db
          .prepare(
            "INSERT INTO questions (id, block_id, prompt, position, created_at) VALUES (?, ?, ?, ?, ?)",
          )
          .bind(questionId, blockId, question.prompt, questionIndex, now),
      );
      question.options.forEach((label, optionIndex) => {
        statements.push(
          this.db
            .prepare(
              "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
            )
            .bind(
              crypto.randomUUID(),
              questionId,
              label,
              optionIndex,
              now,
            ),
        );
      });
    });
    return statements;
  }

  private toAdminBase(row: AdminBlockRow) {
    return {
      id: row.id,
      slug: row.slug,
      title: row.title,
      description: row.description,
      status: row.status,
      createdAt: row.created_at,
      updatedAt: row.updated_at,
      publishedAt: row.published_at,
      closedAt: row.closed_at,
    };
  }
}
