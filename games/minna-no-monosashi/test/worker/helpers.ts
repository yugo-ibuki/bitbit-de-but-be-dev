type SeedBlockInput = {
  id: string;
  slug: string;
  status: "draft" | "published" | "closed";
};

const NOW = "2026-07-20T00:00:00.000Z";

export async function seedBlock(
  db: D1Database,
  input: SeedBlockInput,
): Promise<void> {
  await db
    .prepare(
      `INSERT INTO blocks (
        id, slug, title, description, status, created_at, updated_at,
        published_at, closed_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    )
    .bind(
      input.id,
      input.slug,
      `${input.slug} title`,
      `${input.slug} description`,
      input.status,
      NOW,
      NOW,
      input.status === "draft" ? null : NOW,
      input.status === "closed" ? NOW : null,
    )
    .run();
}

export async function seedPublishedQuestion(db: D1Database): Promise<void> {
  await seedBlock(db, { id: "live", slug: "live", status: "published" });
  await db.batch([
    db
      .prepare(
        "INSERT INTO questions (id, block_id, prompt, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("question-2", "live", "休日は何時から？", 2, NOW),
    db
      .prepare(
        "INSERT INTO questions (id, block_id, prompt, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("question-1", "live", "朝早いのは？", 1, NOW),
    db
      .prepare(
        "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("option-2", "question-1", "7時", 2, NOW),
    db
      .prepare(
        "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("option-1", "question-1", "6時", 1, NOW),
    db
      .prepare(
        "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("option-3", "question-2", "9時", 1, NOW),
    db
      .prepare(
        "INSERT INTO options (id, question_id, label, position, created_at) VALUES (?, ?, ?, ?, ?)",
      )
      .bind("option-4", "question-2", "10時", 2, NOW),
  ]);
}
