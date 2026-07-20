import { allocatePercentages } from "../../domain/percentages";
import type {
  AnswerOutcome,
  QuestionResult,
} from "../../shared/contracts";
import { AppError } from "../http/errors";
import { ResponseRepository } from "../repositories/responseRepository";

export class ResponseService {
  private readonly repository: ResponseRepository;

  constructor(db: D1Database) {
    this.repository = new ResponseRepository(db);
  }

  async answer(input: {
    slug: string;
    questionId: string;
    optionId: string;
    voterHash: string;
  }): Promise<AnswerOutcome> {
    const context = await this.repository.findQuestionContext(
      input.slug,
      input.questionId,
      input.optionId,
    );
    if (!context) {
      throw new AppError(404, "NOT_FOUND", "質問が見つかりません");
    }
    if (context.status !== "published") {
      throw new AppError(409, "BLOCK_NOT_OPEN", "このブロックは回答受付中ではありません");
    }
    if (!context.optionId) {
      throw new AppError(400, "INVALID_OPTION", "この質問の選択肢ではありません");
    }

    const existing = await this.repository.findExisting(
      input.questionId,
      input.voterHash,
    );
    if (existing) {
      return this.resolveExisting(input.questionId, existing, input.optionId);
    }

    try {
      await this.repository.insert({
        blockId: context.blockId,
        questionId: input.questionId,
        optionId: input.optionId,
        voterHash: input.voterHash,
      });
    } catch (error) {
      const raced = await this.repository.findExisting(
        input.questionId,
        input.voterHash,
      );
      if (!raced) throw error;
      return this.resolveExisting(input.questionId, raced, input.optionId);
    }

    return this.buildResult(input.questionId, input.optionId, true);
  }

  async questionResult(input: {
    slug: string;
    questionId: string;
    voterHash: string | null;
  }): Promise<QuestionResult> {
    const context = await this.repository.findQuestionContext(
      input.slug,
      input.questionId,
    );
    if (!context || context.status === "draft") {
      throw new AppError(404, "NOT_FOUND", "質問が見つかりません");
    }

    const selectedOptionId = input.voterHash
      ? await this.repository.findExisting(input.questionId, input.voterHash)
      : null;
    if (context.status === "published" && !selectedOptionId) {
      throw new AppError(403, "RESULTS_LOCKED", "回答後に結果を確認できます");
    }
    return selectedOptionId === null
      ? this.buildResult(input.questionId, null, false)
      : this.buildResult(input.questionId, selectedOptionId, false);
  }

  async progress(slug: string, voterHash: string): Promise<string[]> {
    const block = await this.repository.findPublicBlock(slug);
    if (!block) {
      throw new AppError(404, "NOT_FOUND", "ブロックが見つかりません");
    }
    return this.repository.progress(block.id, voterHash);
  }

  async closedBlockResults(slug: string): Promise<QuestionResult[]> {
    const block = await this.repository.findPublicBlock(slug);
    if (!block) {
      throw new AppError(404, "NOT_FOUND", "ブロックが見つかりません");
    }
    if (block.status !== "closed") {
      throw new AppError(403, "RESULTS_LOCKED", "終了後に全体結果を確認できます");
    }
    const questionIds = await this.repository.listQuestionIds(block.id);
    return Promise.all(
      questionIds.map((questionId) =>
        this.buildResult(questionId, null, false),
      ),
    );
  }

  private async resolveExisting(
    questionId: string,
    existingOptionId: string,
    requestedOptionId: string,
  ): Promise<AnswerOutcome> {
    if (existingOptionId !== requestedOptionId) {
      throw new AppError(409, "ANSWER_LOCKED", "回答は変更できません");
    }
    return this.buildResult(questionId, existingOptionId, false);
  }

  private buildResult(
    questionId: string,
    selectedOptionId: string,
    created: boolean,
  ): Promise<AnswerOutcome>;
  private buildResult(
    questionId: string,
    selectedOptionId: null,
    created: boolean,
  ): Promise<QuestionResult>;
  private async buildResult(
    questionId: string,
    selectedOptionId: string | null,
    created: boolean,
  ): Promise<QuestionResult | AnswerOutcome> {
    const aggregated = await this.repository.aggregate(questionId);
    const percentages = allocatePercentages(
      aggregated.map((option) => option.count),
    );
    const base: QuestionResult = {
      questionId,
      selectedOptionId,
      totalResponses: aggregated.reduce(
        (total, option) => total + option.count,
        0,
      ),
      options: aggregated.map((option, index) => ({
        ...option,
        percentage: percentages[index] ?? 0,
      })),
    };
    return selectedOptionId === null ? base : { ...base, selectedOptionId, created };
  }
}
