import type {
  BlockStatus,
  DraftBlockInput,
  ValidationIssue,
} from "./types";

export const normalizeText = (value: string): string =>
  value.trim().replace(/\s+/g, " ");

export const canTransition = (
  from: BlockStatus,
  to: BlockStatus,
): boolean =>
  (from === "draft" && to === "published") ||
  (from === "published" && to === "closed");

export function validateDraft(input: DraftBlockInput): ValidationIssue[] {
  const issues: ValidationIssue[] = [];
  const title = normalizeText(input.title);
  const description = normalizeText(input.description);

  if (!title || title.length > 80) {
    issues.push({ field: "title", message: "タイトルは1〜80文字で入力してください" });
  }
  if (!/^[a-z0-9-]{3,64}$/.test(input.slug)) {
    issues.push({
      field: "slug",
      message: "スラッグは小文字英数字とハイフンの3〜64文字です",
    });
  }
  if (description.length > 400) {
    issues.push({ field: "description", message: "説明は400文字以内です" });
  }
  if (input.questions.length < 1 || input.questions.length > 10) {
    issues.push({ field: "questions", message: "質問数は1〜10問です" });
  }

  input.questions.forEach((question, index) => {
    const prompt = normalizeText(question.prompt);
    if (!prompt || prompt.length > 200) {
      issues.push({
        field: `questions.${index}.prompt`,
        message: "質問は1〜200文字です",
      });
    }

    const normalizedOptions = question.options.map(normalizeText);
    const hasInvalidOption = normalizedOptions.some(
      (option) => !option || option.length > 80,
    );
    const hasDuplicate =
      new Set(normalizedOptions).size !== normalizedOptions.length;
    if (
      normalizedOptions.length < 2 ||
      hasInvalidOption ||
      hasDuplicate
    ) {
      issues.push({
        field: `questions.${index}.options`,
        message: "選択肢は重複しない1〜80文字を2個以上入力してください",
      });
    }
  });

  return issues;
}
