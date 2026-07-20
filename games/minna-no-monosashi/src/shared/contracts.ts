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
