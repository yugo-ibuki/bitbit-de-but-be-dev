export type BlockStatus = "draft" | "published" | "closed";

export type DraftQuestionInput = {
  prompt: string;
  options: string[];
};

export type DraftBlockInput = {
  title: string;
  slug: string;
  description: string;
  questions: DraftQuestionInput[];
};

export type ValidationIssue = {
  field: string;
  message: string;
};
