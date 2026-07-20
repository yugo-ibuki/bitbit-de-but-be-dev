import { validateDraft } from "../../domain/blockRules";
import type { DraftBlockInput } from "../../domain/types";
import type {
  AdminBlock,
  AdminBlockSummary,
  PublicBlockDetail,
  PublicBlockSummary,
} from "../../shared/contracts";
import { AppError } from "../http/errors";
import { BlockRepository } from "../repositories/blockRepository";

export class BlockService {
  private readonly repository: BlockRepository;

  constructor(db: D1Database) {
    this.repository = new BlockRepository(db);
  }

  listPublic(): Promise<PublicBlockSummary[]> {
    return this.repository.listPublic();
  }

  findPublicBySlug(slug: string): Promise<PublicBlockDetail | null> {
    return this.repository.findPublicBySlug(slug);
  }

  listAdmin(): Promise<AdminBlockSummary[]> {
    return this.repository.listAdmin();
  }

  async findAdminById(id: string): Promise<AdminBlock> {
    const block = await this.repository.findAdminById(id);
    if (!block) throw new AppError(404, "NOT_FOUND", "ブロックが見つかりません");
    return block;
  }

  async createDraft(input: DraftBlockInput): Promise<AdminBlock> {
    try {
      return await this.repository.createDraft(input);
    } catch (error) {
      this.throwSlugConflict(error);
    }
  }

  async updateDraft(id: string, input: DraftBlockInput): Promise<AdminBlock> {
    const existing = await this.findAdminById(id);
    if (existing.status !== "draft") {
      throw new AppError(409, "BLOCK_LOCKED", "公開後の内容は変更できません");
    }
    try {
      const block = await this.repository.updateDraft(id, input);
      if (!block) throw new AppError(404, "NOT_FOUND", "ブロックが見つかりません");
      return block;
    } catch (error) {
      this.throwSlugConflict(error);
    }
  }

  async deleteDraft(id: string): Promise<void> {
    const result = await this.repository.deleteDraft(id);
    if (result === "not-found") {
      throw new AppError(404, "NOT_FOUND", "ブロックが見つかりません");
    }
    if (result === "locked") {
      throw new AppError(409, "BLOCK_LOCKED", "公開後のブロックは削除できません");
    }
  }

  async publish(id: string): Promise<AdminBlock> {
    const block = await this.findAdminById(id);
    if (block.status !== "draft") {
      throw new AppError(409, "INVALID_TRANSITION", "このブロックは公開できません");
    }
    const issues = validateDraft({
      title: block.title,
      slug: block.slug,
      description: block.description,
      questions: block.questions.map((question) => ({
        prompt: question.prompt,
        options: question.options.map((option) => option.label),
      })),
    });
    if (issues.length > 0) {
      throw new AppError(
        422,
        "VALIDATION_FAILED",
        "公開条件を満たしていません",
        issues,
      );
    }
    const published = await this.repository.transition(
      id,
      "draft",
      "published",
    );
    if (!published || published.status !== "published") {
      throw new AppError(409, "INVALID_TRANSITION", "公開状態を更新できませんでした");
    }
    return published;
  }

  async close(id: string): Promise<AdminBlock> {
    const block = await this.findAdminById(id);
    if (block.status !== "published") {
      throw new AppError(409, "INVALID_TRANSITION", "公開中のブロックだけ終了できます");
    }
    const closed = await this.repository.transition(id, "published", "closed");
    if (!closed || closed.status !== "closed") {
      throw new AppError(409, "INVALID_TRANSITION", "終了状態を更新できませんでした");
    }
    return closed;
  }

  async clone(id: string): Promise<AdminBlock> {
    const source = await this.findAdminById(id);
    const baseSlug = `${source.slug}-copy`;
    let slug = baseSlug;
    let suffix = 2;
    while (await this.repository.slugExists(slug)) {
      slug = `${baseSlug}-${suffix}`;
      suffix += 1;
    }
    return this.createDraft({
      title: `${source.title}（コピー）`,
      slug,
      description: source.description,
      questions: source.questions.map((question) => ({
        prompt: question.prompt,
        options: question.options.map((option) => option.label),
      })),
    });
  }

  private throwSlugConflict(error: unknown): never {
    if (error instanceof Error && /UNIQUE constraint failed: blocks\.slug/.test(error.message)) {
      throw new AppError(409, "SLUG_CONFLICT", "同じURL識別子がすでに使われています");
    }
    throw error;
  }
}
