export type PublicBlockStatus = "published" | "closed";

export type PublicBlockSummary = {
  slug: string;
  title: string;
  description: string;
  status: PublicBlockStatus;
  questionCount: number;
  participantCount: number;
};

export type PublicOption = {
  id: string;
  label: string;
  position: number;
};

export type PublicQuestion = {
  id: string;
  prompt: string;
  position: number;
  options: PublicOption[];
};

export type PublicBlockDetail = PublicBlockSummary & {
  questions: PublicQuestion[];
};

export type ApiErrorBody = {
  error: { code: string; message: string };
};

export type QuestionResultOption = {
  id: string;
  label: string;
  count: number;
  percentage: number;
};

export type QuestionResult = {
  questionId: string;
  selectedOptionId: string | null;
  totalResponses: number;
  options: QuestionResultOption[];
};

export type AnswerOutcome = QuestionResult & {
  created: boolean;
  selectedOptionId: string;
};
