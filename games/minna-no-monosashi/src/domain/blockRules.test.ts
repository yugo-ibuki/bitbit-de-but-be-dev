import { canTransition, validateDraft } from "./blockRules";

const valid = {
  title: "日常の境界線",
  slug: "daily-boundaries",
  description: "",
  questions: [{ prompt: "朝早いのは？", options: ["6時", "7時"] }],
};

it("accepts 1 to 10 questions and at least 2 unique options", () => {
  expect(validateDraft(valid)).toEqual([]);
  expect(validateDraft({ ...valid, questions: [] })).toContainEqual(
    expect.objectContaining({ field: "questions" }),
  );
  expect(
    validateDraft({
      ...valid,
      questions: Array.from({ length: 11 }, () => valid.questions[0]),
    }),
  ).toContainEqual(expect.objectContaining({ field: "questions" }));
  expect(
    validateDraft({
      ...valid,
      questions: [{ prompt: "朝早いのは？", options: ["6時", " 6時 "] }],
    }),
  ).toContainEqual(expect.objectContaining({ field: "questions.0.options" }));
});

it("only allows draft to published and published to closed", () => {
  expect(canTransition("draft", "published")).toBe(true);
  expect(canTransition("published", "closed")).toBe(true);
  expect(canTransition("closed", "published")).toBe(false);
});
