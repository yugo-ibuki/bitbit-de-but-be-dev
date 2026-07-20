import type {
  PublicBlockDetail,
  PublicBlockStatus,
  PublicBlockSummary,
  PublicOption,
  PublicQuestion,
} from "../../shared/contracts";

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
}
